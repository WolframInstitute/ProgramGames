#!/bin/bash
set -e

# Cross-compilation build script for program-games
# Builds release binaries for Linux and Windows platforms from Linux.
# macOS targets are built natively on macOS (see build_macos_targets.sh)
# to enable Metal GPU support.

# Define targets: WolframSystemID:Rust_target:extra_cargo_flags
TARGETS=(
    "Linux-x86-64:x86_64-unknown-linux-gnu:--no-default-features"
    "Linux-ARM64:aarch64-unknown-linux-gnu:--no-default-features"
    "Windows-x86-64:x86_64-pc-windows-gnu:--no-default-features"
)

echo "Building program-games for Linux/Windows targets..."
echo

for entry in "${TARGETS[@]}"; do
    IFS=':' read -r system_id target extra_flags <<< "$entry"
    echo "=== Building for $system_id ($target) ==="

    cmd=(cargo build --release --target "$target")
    if [ -n "$extra_flags" ]; then
        cmd+=($extra_flags)
    fi

    if "${cmd[@]}"; then
        echo "OK $system_id build succeeded"
    else
        echo "FAIL $system_id build failed"
        exit 1
    fi
    echo
done

echo "=== All builds completed successfully ==="

# Show output locations
echo
echo "Built libraries:"
for entry in "${TARGETS[@]}"; do
    IFS=':' read -r system_id target _ <<< "$entry"
    case "$target" in
        *-windows-*)
            ext="dll"
            ;;
        *-apple-*)
            ext="dylib"
            ;;
        *)
            ext="so"
            ;;
    esac
    echo "  $system_id: target/$target/release/libprogram_games.$ext"
done
