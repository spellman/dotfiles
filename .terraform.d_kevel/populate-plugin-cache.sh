#!/usr/bin/env bash
# Populate ~/.terraform.d/plugin-cache with extra platforms for every
# provider+version already present there.
#
# Walks the cache, finds each (namespace, name, version), and for each
# target platform asks the Terraform registry for the download URL,
# fetches the zip, and extracts into the cache layout terraform expects.
# Skips entries that are already populated or whose platform the
# provider does not publish.

set -u

CACHE="${TF_PLUGIN_CACHE_DIR:-$HOME/.terraform.d/plugin-cache}"
REGISTRY_HOST="registry.terraform.io"
PLATFORMS=("linux_amd64" "linux_arm64")

if [[ ! -d "$CACHE/$REGISTRY_HOST" ]]; then
    echo "No cache found at $CACHE/$REGISTRY_HOST" >&2
    exit 1
fi

total_fetched=0
total_skipped=0
total_unavailable=0
total_failed=0

# Each <namespace>/<name>/<version> directory in the cache.
while IFS= read -r version_dir; do
    rel="${version_dir#"$CACHE/$REGISTRY_HOST/"}"
    namespace="${rel%%/*}"
    rest="${rel#*/}"
    name="${rest%%/*}"
    version="${rest#*/}"

    for platform in "${PLATFORMS[@]}"; do
        os="${platform%_*}"
        arch="${platform#*_}"
        target_dir="$version_dir/$platform"

        if compgen -G "$target_dir/terraform-provider-*" > /dev/null; then
            printf "skip      %s/%s %s %s (already cached)\n" \
                "$namespace" "$name" "$version" "$platform"
            total_skipped=$((total_skipped + 1))
            continue
        fi

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

        tmp_zip="$(mktemp -t tfprov.XXXXXX.zip)"
        if ! curl -sfL "$download_url" -o "$tmp_zip"; then
            printf "FAIL      %s/%s %s %s (download error)\n" \
                "$namespace" "$name" "$version" "$platform"
            rm -f "$tmp_zip"
            total_failed=$((total_failed + 1))
            continue
        fi

        mkdir -p "$target_dir"
        if ! unzip -q -o "$tmp_zip" -d "$target_dir"; then
            printf "FAIL      %s/%s %s %s (unzip error)\n" \
                "$namespace" "$name" "$version" "$platform"
            rm -f "$tmp_zip"
            rmdir "$target_dir" 2>/dev/null || true
            total_failed=$((total_failed + 1))
            continue
        fi
        rm -f "$tmp_zip"
        total_fetched=$((total_fetched + 1))
    done
done < <(find "$CACHE/$REGISTRY_HOST" -mindepth 3 -maxdepth 3 -type d | sort)

echo ""
echo "fetched:        $total_fetched"
echo "already cached: $total_skipped"
echo "not published:  $total_unavailable"
echo "failed:         $total_failed"
