# Windows Setup Instructions
`<project root>` below refers to wherever this git repository was cloned to.

## Configure
1. Install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
1. `stack exec bash` then in the msys2 bash shell run:
   - `pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_image mingw-w64-x86_64-SDL2_ttf`

#### Enable Game Audio (optional)
1. Register for a [FMOD account](https://www.fmod.com/profile/register)
1. Download and install ["FMOD Studio Studio Suite - FMOD Engine 2.02"](https://www.fmod.com/download#fmodstudiosuite) (Windows)
1. From the installed contents:
   - Copy `FMOD Studio API Windows/api/core/inc/*.h` into `<project root>/inc/`
   - Copy `FMOD Studio API Windows/api/studio/inc/*.h` into `<project root>/inc/`
   - Copy `FMOD Studio API Windows/api/core/lib/x64/fmod.dll` into `<project root>/lib/`
   - Copy `FMOD Studio API Windows/api/studio/lib/x64/fmodstudio.dll` into `<project root>/lib/`
1. Edit the lines at the bottom of `<project root>/package.yaml`:
   - In the `c-sources:` line change `wrapper-NOAUDIO.c` to `wrapper.c`
   - Uncomment the `extra-lib-dirs` and `extra-libraries` lines

## Build
`stack build --local-bin-path . --copy-bins` (ignore any warnings about PATH)

## Run
This assumes you've purchased a copy of the [game on Steam](https://store.steampowered.com/app/1136730/Defect_Process/).

1. From the Steam client right click the Defect Process entry in the games library `-> Properties -> Betas`
   - Change the selected beta from "None" to "github - branch for github"
1. From the Steam client right click the Defect Process entry in your games library `-> Properties -> Local Files -> Browse`. The file explorer window that pops up should be in the installed steam game directory:
   - Copy the contents of `<steam game directory>/data/` into `<project root>/data/`
   - Copy `<steam game directory>/*.dll` into `<project root>/`
1. From `<project root>` run `defect-process.exe`

Heavy use of the the in-game dev console is needed to try out most things, due to the omitted code. See [docs/dev-console-reference.pdf](https://github.com/incoherentsoftware/defect-process/blob/main/docs/dev-console-reference.pdf) for what commands are available. It's recommended to play the game normally on Steam for a bit to get a feel for how it works.
