# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]


## [1.2.1] - 2025-11-23

### Fixed

- Correctly use tree sitter wgsl grammar for WGSL (not used yet).
- Fix some link in doc.
- Fix for a crash with an unreachable tag that can be reached in some case.
- Fix path remapping which canonicalize path, making them useless on Windows.

### Changed

- Improved triple information at startup for debug purpose.


## [1.2.0] - 2025-10-05

### Added

- Added documentations for crates on docs.rs with test doc.
- Added support for cancel request.
- Added the possibility to override the server configuration with custom JSON for engine specific configuration.
- Added the possibility to set server cwd in command line.
- Added setting for defining macro per shader stage.

### Fixed

- Fixed some invalid macro description on hover
- Fixed som template failing to access members.
- Reduced memory footprint of symbol requirement.

### Changed

- Lru cache is now created with small size and resized if needed to reduce memory footprint.
- Added a dedicated error enum for language server to not have server logic into shader sense.
- Server now add workspace folders for includes. This should help finding file in workspace with relative path.
- BREAKING: shader stage should now be passed in camelCase.
- Improved the way we look for DXC DLL to avoid picking mismatching DLL between dxcompiler and dxil.


## [1.1.0] - 2025-08-31

### Added

- Hlsl raytracing functions added to database
- Hlsl enum support (syntax highlighting + hover + completion)
- Glsl mesh shader extension added to database. Feel free to contribute to this database by adding other extensions !
- LRU cache for semantic tokens regex which are created on the fly and were impacting performances.

### Changed

- Upgraded tree-sitter dependencies to use tree-sitter-language which allow using different languages easily such as slang. Wgsl does not support it yet, so waiting for [this PR](https://github.com/tree-sitter-grammars/tree-sitter-wgsl-bevy/pull/19) to upgrade.

### Fixed
- Validation was not retriggered when changing settings.
- Stripped template from variable type to help finding in symbol list.

## [1.0.0] - 2025-08-25

This new version features a lot of changes that should drastically improve performances on huge shader codebase, memory usage and behaviour of variant & updates. There was also some redesign with the architecture to handle this and make the API more friendly to use.

### Fixed

- Fixed an issue where regions were not displayed when diagnostics were disabled.
- Fixed an issue with file using variant context missing some symbols behind include guards.
- Fixed and improved the way symbols caching are handled.
- Fixed a warning when hovering a field return by a method. 
- Fixed an issue whith HLSL database where its missing some parameters in Texture objects.
- Fixed an issue with include handler pushing include from inactive region on stack, which could end up including file from a wrong folder.
- Fixed intrinsics depending on requirements that were ignored.
- Fixed some missing diagnostics clear when files are closed.
- Fixed some dependency files not being cleared from memory when main file was closed.
- Fixed an issue with updating a variant dependency that would cause it to clear inactive regions.
- Fixed diagnostic not being cleared when passing from error state to non error state.
- Fixed an issue where a variant file not opened prevent using the feature.
 
### Changed

- There can be only **one** variant at a time to avoid file randomly picking a variant.
- ValidationParams & SymbolParams have been merged into a single struct ShaderParams.
- Update messages are now handled asynchronously, which avoid messages from getting queued when caching take a lot of time. This can increase server speed drastically in huge codebase.
- Reporting cache update through progress report notification.
- Validator is now behind a solid struct instead of a trait.
- Main functions are now sharing common entry point for uniformity.
- Reduced memory usage of the server.
- Changed API entry point.
- Clean code, moved files & renaming

### Added

- Added Dxc builtins macros to symbols.
- Added advanced symbols filterings for intrinsics. Extensions should be now easy to add for GLSL in an upcoming release.
- Added test for each shader stage.
- Added check for which shader stage is supported in Validator
- Added command line argument for shader-language-server to pass configuration on startup.

## [0.8.2] - 2025-08-03

### Fixed

- Fixed a crash when DXC fail to instantiate. Fallback to glslang for validation instead.
- Fixed an issue with dxil dll loading on Linux as dxcompiler.so seems to load it from path while we create it from absolute path.

## [0.8.1] - 2025-08-03

### Fixed

- Fixed an issue with invalid position computed using UTF8 characters
- Fixed a crash when triggering completion on position at start of line due to invalid line computation from byte offset.
- Updated shader-sense-cli Cargo.toml for a crates.io release.

## [0.8.0] - 2025-08-03

### Added

- **Struct completion** features has been hugely improved. You will now benefit of structure field completion in HLSL & GLSL.
- HLSL database now include methods for Texture & Buffer Objects to be used along struct completion.
- Added **formatting** for HLSL & GLSL code through clang format (for full & partial formatting). Support for a .clang-format configuration file.
- Added an option for **DXC SPIRV** support to remove warning related to SPIRV (shader-validator.hlsl.spirv).
- Every field of the config is now optional for third party client and server will not crash if its missing some.
- Improvement on caching and dependencies handling aswell as performances.
- Add a CLI tool for validating shaders.

### Fixed
- Fix an issue where DXC warning were not displayed if no error was found.
- Fix an issue where DXC was picking globally accessible DXC instead of bundled one.
- Fix an issue with region missing some defines.
- Fix an issue with dirty files being wrongly tracked.
- Fix an issue with HLSL array variable in struct not being captured.


## [0.7.0] - 2025-07-22

### Added

- Function parameters will get highlighted through semantic tokens
- Error from dependencies should be easier to track down.

### Fixed

- Pragma once files failing to load dependencies
- Inlay hint being displayed for every dependencies in main file


## [0.6.2] - 2025-06-06

### Fixed

- Fix an issue preventing dependencies to be updated correctly when opened in editor.
- Add guard to prevent crash on include stack overflow
- Improved outline display by using DocumentSymbol instead of SymbolInformation on document symbol request. This allow vscode to know if cursor is inside a function for example and contain more informations.


## [0.6.1] - 2025-05-12

### Fixed

- Small improvements to CI to better improve support for all possible platforms.
- Do not build mac server version as DXC does not publish mac binaries.

## [0.6.0] - 2025-05-03

### Added

- Variant now have context. When you open a dependency of the variant, it will have its context from the first place its included. It means it will have macro definition passed from context.
- Symbols now store directly there include data which simplify a lot the way we interact with it. More logic moved from shader-language-server into shader-sense directly.
- Fix performance issues with huge diagnostic parsing.
- Update opened files when a dependency file is being updated.
- Added some missing constructor override for completion & other providers.
- Improved testing api for server.
- Allow virtual path to omit / at the beginning.
- Some improvement for semantic tokens symbols from other files.


## [0.5.5] - 2025-04-04

### Added

- Inlay hints are now displayed for function call. They can be toggled via vscode settings "editor.inlayHints.enable"
- Constructors are now handled correctly and display their signature.
- Add support for array variable in HLSL.

### Fixed

- Better handling of updates with improved performances
- Fix an issue where editing a file would remove symbols from included files from all providers.


## [0.5.4] - 2025-03-26

### Fixed

- Hide the new symbol provider diagnostic feature behind an option as it may be quite invasive.
- Fix invalid range given to hover provider


## [0.5.3] - 2025-03-23

### Fixed

- Improved the define context for included files that should behave more correctly.
- Improved performances by removing unnecessary computations from process
- Improved the way includes are resolved.
- Add support for angled bracket in GLSL.
- Add a command to dump the dependency tree in logs.
- Add error from symbol parsing issues as warning to user.
- Fix a leak when closing files did not released it on server.


## [0.5.2] - 2025-03-09

### Fixed

- Fix some UTF8 encoding issues that caused server to crash when typing UTF8 characters.
- Updated Naga to latest
- Improved logs & profiling.
- Improved folding ranges that missed all curly braces scopes.
- Fix Linux test issue with EOL.


## [0.5.1] - 2025-03-04

### Added

- GLSL is more on par with HLSL (support regions aswell now)
- Added uniform block to symbols for GLSL
- Add folding range provider for regions.

### Fixed

- Fix files with missing stages way of validating them in GLSL (dont rely on include extension).
- Fix a crash when #pragma once is first in the file.


## [0.5.0] - 2025-03-01

### Added

- Now detect inactive regions due to preprocessor and filter symbols and includes from it (HLSL only).
- Improved macro & includes detection and tracking.
- Improved informations returned to user when hovering.
- Added support for virtual path via configuration.
- Added a variant system that allow to define entry point with a specific entry point & macros. System will be improved with time.
- Restore test for wasi web target.
- Add document symbol provider.
- Add workspace symbol provider.
- Add semantic token provider for defined macros.

### Fixed

- Fixed a crash when pasting content at the end of file.
- Fixed a bug for client that do not send configuration.
- Fixed a bug where diagnostics would not trigger after configuration update.
- Fixed & impoved some HLSL queries

## [0.4.2] - 2025-01-03

### Added

- Cross platform compilation of server on windows / linux / macOS (note that macOS has not been tested as I dont have the tools for it)
- Add a Dump AST command to help debug tree-sitter queries
- Add sample for shader-sense
- Clean and fix readmes

## [0.4.1] - 2024-12-20

### Fixed

- Fix invalid URL format for desktop that cause invalid tracking of files.
- Improved error tracking.
- Improved struct completion system in HLSL.
- Fix Unknown type issue for vector type in HLSL.

## [0.4.0] - 2024-11-23

### Added

- [Tree sitter](https://tree-sitter.github.io/tree-sitter/) symbol querying backend instead of Regex.
- Struct field completion
- More stable symbol query
- Easily add languages and complex features
- Includes are now browsable by CTRL+click
- Symbols have range instead of position, so better hovering & goto experience.
- Setting `validateOnType` & `validateOnSave` replaced by validate
- Setting `autocomplete` replaced by symbols.

## [0.3.1] - 2024-10-02

### Added

- Release the server on [crates.io](https://crates.io/crates/shader_language_server)

## [0.3.0] - 2024-09-30

### Added

- HLSL intrinsics database is now completed.
- Select HLSL version for diagnostic.
- Enable 16 bit support for HLSL
- Diagnostics errors are now sent to user.

### Fixed

## [0.2.4] - 2024-09-23

### Added

- Add settings for dxc shader model & glslang target.
- Cache intrinsics for improved performances

## [0.2.3] - 2024-09-22

### Fixed

- Fix hanging WASI server by upgrading WASI SDK

## [0.2.2] - 2024-09-22

### Fixed

- Fix crash for web version of extension because of std::fs::canonicalize not supported

## [0.2.1] - 2024-09-21

### Fixed

- Fix a crash with relative path conversion to URL

## [0.2.0] - 2024-09-07

### Added

- Add scope detection for symbol provider. It will now provides only symbols that are accessibles from a given scope.
- Add HLSL symbol provider. Local symbols are now provided, but intrinsics symbols are only partially provided for now.

## [0.1.3] - 2024-08-28

### Added

- Add custom macro to completion
- Add command line argument `--version` to display server version

## [0.1.2] - 2024-08-27

### Fixed

- Fix crash with relative include failing to be resolved and panicking the server.

## [0.1.1] - 2024-08-26

### Fixed

- Fix a server crash when completing code with HLSL / WGSL
- Fix function symbol regex matching issues

### Changed

- Code modularization
- Test consolidation

## [0.1.0] - 2024-08-25

### Added

- Language server is now based on [LSP protocol](https://microsoft.github.io/language-server-protocol/) which allow this server to be used by any client following this protocol.
- Autocompletion
- Signature helper
- Hover
- Go to definition

## [0.0.4] - 2024-07-29

### Fixed

- Consolidate server & some cleans.

## [0.0.3] - 2024-07-20

### Added

- glslang now display column errors

## [0.0.2] - 2024-07-18

### Fixed

- Fixes & improvement for glslang mostly which now support includes & macros.

## [0.0.1] - 2024-07-14

Initial release of this extension


<!-- Below are link for above changelog titles-->
[unreleased]: https://github.com/antaalt/shader-sense/compare/v1.2.1...HEAD
[1.2.0]: https://github.com/antaalt/shader-sense/compare/v1.2.0...v1.2.1
[1.2.0]: https://github.com/antaalt/shader-sense/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/antaalt/shader-sense/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/antaalt/shader-sense/compare/v0.8.2...v1.0.0
[0.8.2]: https://github.com/antaalt/shader-sense/compare/v0.8.1...v0.8.2
[0.8.1]: https://github.com/antaalt/shader-sense/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/antaalt/shader-sense/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/antaalt/shader-sense/compare/v0.6.1...v0.7.0
[0.6.2]: https://github.com/antaalt/shader-sense/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/antaalt/shader-sense/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/antaalt/shader-sense/compare/v0.5.5...v0.6.0
[0.5.5]: https://github.com/antaalt/shader-sense/compare/v0.5.4...v0.5.5
[0.5.4]: https://github.com/antaalt/shader-sense/compare/v0.5.3...v0.5.4
[0.5.3]: https://github.com/antaalt/shader-sense/compare/v0.5.2...v0.5.3
[0.5.2]: https://github.com/antaalt/shader-sense/compare/v0.5.1...v0.5.2
[0.5.1]: https://github.com/antaalt/shader-sense/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/antaalt/shader-sense/compare/v0.4.2...v0.5.0
[0.4.2]: https://github.com/antaalt/shader-sense/compare/v0.4.1...v0.4.2
[0.4.1]: https://github.com/antaalt/shader-sense/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/antaalt/shader-sense/compare/v0.3.1...v0.4.0
[0.3.1]: https://github.com/antaalt/shader-sense/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/antaalt/shader-sense/compare/v0.2.4...v0.3.0
[0.2.4]: https://github.com/antaalt/shader-sense/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/antaalt/shader-sense/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/antaalt/shader-sense/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/antaalt/shader-sense/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/antaalt/shader-sense/compare/v0.1.3...v0.2.0
[0.1.3]: https://github.com/antaalt/shader-sense/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/antaalt/shader-sense/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/antaalt/shader-sense/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/antaalt/shader-sense/compare/v0.0.4...v0.1.0
[0.0.4]: https://github.com/antaalt/shader-sense/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/antaalt/shader-sense/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/antaalt/shader-sense/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/antaalt/shader-sense/releases/tag/v0.0.1