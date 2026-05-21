#!/usr/bin/env bash
# Thin wrapper around terraform that injects `-fs-mirror=$TF_MIRROR_DIR`
# into `terraform providers lock` invocations.
#
# Why: `terraform providers lock` ignores filesystem_mirror entries in
# provider_installation and always contacts the origin registry to fetch
# signed SHA256SUMS files — by design, since the lock command's job is
# to record publisher-authenticated hashes. The `-fs-mirror=DIR` command-
# line flag is the only knob that overrides this, telling lock to trust
# the named mirror as the authoritative source.
#
# cdktf invokes `terraform providers lock -platform=linux_amd64` after
# each init and honors $TERRAFORM_BINARY_NAME for which terraform to run,
# so pointing $TERRAFORM_BINARY_NAME at this wrapper makes the mirror
# stick through cdktf's cross-platform lock step. All other terraform
# invocations are passed through unmodified.
#
# Trust-chain note: Terraform's verification has three levels —
#   (1) publisher GPG signature on SHA256SUMS (strongest)
#   (2) hashes recorded in .terraform.lock.hcl
#   (3) registry's TLS certificate (weakest)
# Adding `-fs-mirror` skips the level-1 fetch, so providers come back as "(unauthenticated)" in the output — but the lock-file hash check (level 2) still runs against every binary the mirror serves, and refuses any mismatch. That's the same posture an air-gapped CI deploy uses: trust .terraform.lock.hcl as the root of authority. The risk this introduces is the same risk any deployment takes when running from a committed lock without re-verifying signatures — fine as long as .terraform.lock.hcl is reviewed in PRs the same way other code is.

set -u

# Resolution: REAL_TERRAFORM env var overrides; otherwise the wrapper takes the first `terraform` in PATH. Transparent to tfenv/asdf/Homebrew/manual installs.
REAL_TERRAFORM="${REAL_TERRAFORM:-$(command -v terraform 2>/dev/null || true)}"

if [[ -z "$REAL_TERRAFORM" || ! -x "$REAL_TERRAFORM" ]]; then
    echo "terraform-fs-mirror-wrapper: no executable terraform found in PATH (set REAL_TERRAFORM env var to override)" >&2
    exit 127
fi

# Loop guard. Plain string compare — catches the common foot-gun of REAL_TERRAFORM pointing at this script via the same path it was invoked with. Symlink-mediated loops still slip through, but those require deliberately misconfigured PATH and manifest as fast bash recursion errors rather than silent hangs.
if [[ "$REAL_TERRAFORM" == "$0" ]]; then
    echo "terraform-fs-mirror-wrapper: REAL_TERRAFORM points at this script; set REAL_TERRAFORM=<path-to-real-terraform>" >&2
    exit 1
fi

MIRROR="${TF_MIRROR_DIR:-$HOME/.terraform.d/mirror}"

# Walk positional (non-flag) args until we know the subcommand.
subcommand1=""
subcommand2=""
for arg in "$@"; do
    case "$arg" in
        -*) ;;
        *)
            if [[ -z "$subcommand1" ]]; then
                subcommand1="$arg"
            elif [[ -z "$subcommand2" ]]; then
                subcommand2="$arg"
                break
            fi
            ;;
    esac
done

if [[ "$subcommand1" == "providers" && "$subcommand2" == "lock" ]]; then
    exec "$REAL_TERRAFORM" "$@" "-fs-mirror=$MIRROR"
fi

exec "$REAL_TERRAFORM" "$@"
