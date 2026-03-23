#!/bin/bash
set -e

# Native macOS build script for program-games
# Builds macOS targets with Metal GPU support enabled (default feature).
# Must be run on a macOS host where Metal framework is available.

TARGETS=(
    "MacOSX-x86-64:x86_64-apple-darwin"
    "MacOSX-ARM64:aarch64-apple-darwin"
)

echo "Building program-games for macOS targets (with Metal GPU)..."
echo

for entry in "${TARGETS[@]}"; do
    IFS=':' read -r system_id target <<< "$entry"
    echo "=== Building for $system_id ($target) ==="

    if cargo build --release --target "$target"; then
        echo "OK $system_id build succeeded"
    else
        echo "FAIL $system_id build failed"
        exit 1
    fi
    echo
done

echo "=== All macOS builds completed successfully ==="

echo
echo "Built libraries:"
for entry in "${TARGETS[@]}"; do
    IFS=':' read -r system_id target <<< "$entry"
    echo "  $system_id: target/$target/release/libprogram_games.dylib"
done
