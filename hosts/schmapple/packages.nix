{ pkgs }:

with pkgs; [
  hidden-bar
  darwin.sigtool
  darwin.apple_sdk.frameworks.Security

  (let
    emacs-macport-with-app = pkgs.emacs-macport.overrideAttrs (oldAttrs: {
      configureFlags = (oldAttrs.configureFlags or []) ++ [
        "--with-mac"
        "--enable-mac-app=${placeholder "out"}/Applications"
      ];

      postInstall = (oldAttrs.postInstall or "") + ''
        # Ensure the app bundle was created
        if [ -d "${placeholder "out"}/Applications/Emacs.app" ]; then
          PLIST="${placeholder "out"}/Applications/Emacs.app/Contents/Info.plist"

          # Create or fix Info.plist with proper bundle identifier
          if [ ! -f "$PLIST" ]; then
            # Create Info.plist if it doesn't exist
            mkdir -p "${placeholder "out"}/Applications/Emacs.app/Contents"
            cat > "$PLIST" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleIdentifier</key>
  <string>org.gnu.Emacs</string>
  <key>CFBundleName</key>
  <string>Emacs</string>
  <key>CFBundleDisplayName</key>
  <string>Emacs</string>
  <key>CFBundleExecutable</key>
  <string>Emacs</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>NSHighResolutionCapable</key>
  <true/>
</dict>
</plist>
EOF
          else
            # Fix existing Info.plist if CFBundleIdentifier is missing
            if ! /usr/libexec/PlistBuddy -c "Print :CFBundleIdentifier" "$PLIST" 2>/dev/null; then
              /usr/libexec/PlistBuddy -c "Add :CFBundleIdentifier string org.gnu.Emacs" "$PLIST" 2>/dev/null || true
            fi
          fi

          echo "Emacs.app bundle created with proper Info.plist"
        fi
      '';
    });
  in
    (pkgs.emacsPackagesFor emacs-macport-with-app).emacsWithPackages (epkgs: with epkgs; [
      magit
      consult
      hl-todo
      doom-modeline
    ])
  )

  skimpdf
  # pdfpc
]
