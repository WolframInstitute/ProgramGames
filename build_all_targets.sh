#!/bin/bash
set -e

# Cross-compilation build script for program-games
# Builds release binaries for all supported platforms

# Define targets: WolframSystemID:Rust_target:extra_cargo_flags
# All targets disable default features (metal) since we cross-compile from Linux
# and macOS system frameworks (objc, CoreFoundation) aren't available in the sysroot.
# Metal support is only available when building natively on macOS.
TARGETS=(
    "MacOSX-x86-64:x86_64-apple-darwin:--no-default-features"
    "MacOSX-ARM64:aarch64-apple-darwin:--no-default-features"
    "Linux-x86-64:x86_64-unknown-linux-gnu:--no-default-features"
    "Linux-ARM64:aarch64-unknown-linux-gnu:--no-default-features"
    "Windows-x86-64:x86_64-pc-windows-gnu:--no-default-features"
)

# Install a dummy xcrun so rustc doesn't warn about missing macOS SDK
# when cross-compiling for darwin targets from Linux.
if ! command -v xcrun &> /dev/null; then
    cat > /usr/local/bin/xcrun << 'XCRUN'
#!/bin/bash
echo "/opt/macos-sysroot"
XCRUN
    chmod +x /usr/local/bin/xcrun
fi

echo "Building program-games for all targets..."
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
