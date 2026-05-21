#!/usr/bin/env bash
# Build a filesystem mirror at ~/.terraform.d/mirror covering every
# (provider, version) found in ~/.terraform.d/plugin-cache.
#
# Why this exists: `terraform providers lock` ignores plugin_cache_dir by
# design, so cdktf's cross-platform lock step keeps hitting
# releases.hashicorp.com even when the cache is fully populated. The
# filesystem mirror format, in contrast, IS consulted by `providers lock`
# when configured via `provider_installation` in ~/.terraformrc.
#
# Run this once while ProtonVPN is off (tether if needed) so all the
# zip downloads succeed. Afterwards `terraform providers lock` reads
# everything from disk and the registry is never contacted.
#
# Idempotent: rerunning skips zips already present. Safe to run after
# any new provider version lands in the plugin cache.

set -u

CACHE="${TF_PLUGIN_CACHE_DIR:-$HOME/.terraform.d/plugin-cache}"
MIRROR="${TF_MIRROR_DIR:-$HOME/.terraform.d/mirror}"
REGISTRY_HOST="registry.terraform.io"
PLATFORMS=("darwin_arm64" "linux_amd64" "linux_arm64")

if [[ ! -d "$CACHE/$REGISTRY_HOST" ]]; then
    echo "No cache found at $CACHE/$REGISTRY_HOST" >&2
    exit 1
fi

mkdir -p "$MIRROR/$REGISTRY_HOST"

# h1: hash = base64(sha256(manifest)) where manifest is the sorted
# concatenation of "<hex-sha256-of-file>  <name>\n" for every file inside
# the zip. Same algorithm as golang.org/x/mod/sumdb/dirhash.Hash1. Not
# computable in pure bash, so we shell out to python.
h1_of_zip() {
    local zip="$1"
    /usr/bin/python3 - "$zip" <<'PY'
import sys, zipfile, hashlib, base64
h = hashlib.sha256()
with zipfile.ZipFile(sys.argv[1]) as z:
    for name in sorted(z.namelist()):
        with z.open(name) as f:
            inner = hashlib.sha256(f.read()).hexdigest()
        h.update(f"{inner}  {name}\n".encode())
print("h1:" + base64.b64encode(h.digest()).decode(), end="")
PY
}

# zh: hash = hex(sha256(zip_bytes)). Legacy scheme still recognized by
# Terraform; included alongside h1 so locks that predate h1 still verify.
zh_of_zip() {
    local zip="$1"
    local sha
    sha="$(shasum -a 256 "$zip" | cut -d' ' -f1)"
    printf 'zh:%s' "$sha"
}

total_fetched=0
total_skipped=0
total_unavailable=0
total_failed=0

while IFS= read -r version_dir; do
    rel="${version_dir#"$CACHE/$REGISTRY_HOST/"}"
    namespace="${rel%%/*}"
    rest="${rel#*/}"
    name="${rest%%/*}"
    version="${rest#*/}"

    prov_dir="$MIRROR/$REGISTRY_HOST/$namespace/$name"
    mkdir -p "$prov_dir"

    archive_entries=()

    for platform in "${PLATFORMS[@]}"; do
        os="${platform%_*}"
        arch="${platform#*_}"
        zip_name="terraform-provider-${name}_${version}_${platform}.zip"
        zip_path="$prov_dir/$zip_name"

        if [[ -f "$zip_path" ]]; then
            printf "skip      %s/%s %s %s (zip already mirrored)\n" \
                "$namespace" "$name" "$version" "$platform"
            total_skipped=$((total_skipped + 1))
        else
            api_url="https://$REGISTRY_HOST/v1/providers/$namespace/$name/$version/download/$os/$arch"
            info="$(curl -sf "$api_url" || true)"
            if [[ -z "$info" ]]; then
                printf "no-pub    %s/%s %s %s (not published)\n" \
                    "$namespace" "$name" "$version" "$platform"
                total_unavailable=$((total_unavailable + 1))
                continue
            fi
            download_url="$(printf '%s' "$info" | jq -r '.download_url // empty')"
            if [[ -z "$download_url" ]]; then
                printf "no-url    %s/%s %s %s (registry returned no URL)\n" \
                    "$namespace" "$name" "$version" "$platform"
                total_unavailable=$((total_unavailable + 1))
                continue
            fi

            printf "fetch     %s/%s %s %s\n" \
                "$namespace" "$name" "$version" "$platform"
            if ! curl -sfL "$download_url" -o "$zip_path"; then
                printf "FAIL      %s/%s %s %s (download error)\n" \
                    "$namespace" "$name" "$version" "$platform"
                rm -f "$zip_path"
                total_failed=$((total_failed + 1))
                continue
            fi
            total_fetched=$((total_fetched + 1))
        fi

        h1="$(h1_of_zip "$zip_path")"
        zh="$(zh_of_zip "$zip_path")"
        archive_entries+=("{\"platform\":\"$platform\",\"url\":\"$zip_name\",\"hashes\":[\"$h1\",\"$zh\"]}")
    done

    # <version>.json: maps each platform to its zip URL and h1 hash list
    if [[ ${#archive_entries[@]} -gt 0 ]]; then
        printf '%s\n' "${archive_entries[@]}" \
            | jq -s '{archives: (map({(.platform): {url: .url, hashes: .hashes}}) | add)}' \
            > "$prov_dir/$version.json"
    fi
done < <(find "$CACHE/$REGISTRY_HOST" -mindepth 3 -maxdepth 3 -type d | sort)

# index.json per provider: lists every version this mirror serves
while IFS= read -r prov_dir; do
    versions=()
    for version_json in "$prov_dir"/*.json; do
        [[ -e "$version_json" ]] || continue
        base="$(basename "$version_json" .json)"
        [[ "$base" = "index" ]] && continue
        versions+=("$base")
    done
    if [[ ${#versions[@]} -gt 0 ]]; then
        printf '%s\n' "${versions[@]}" \
            | jq -R . \
            | jq -s '{versions: (map({(.): {}}) | add)}' \
            > "$prov_dir/index.json"
    fi
done < <(find "$MIRROR/$REGISTRY_HOST" -mindepth 2 -maxdepth 2 -type d | sort)

echo ""
echo "fetched:          $total_fetched"
echo "already mirrored: $total_skipped"
echo "not published:    $total_unavailable"
echo "failed:           $total_failed"
echo ""
echo "Mirror at: $MIRROR"
