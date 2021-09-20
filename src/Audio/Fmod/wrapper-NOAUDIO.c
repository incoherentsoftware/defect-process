#define FMOD_BOOL int

const char emptyString[] = "";

int initFmod(char* baseResourceDir) {
    return 0;
}

int freeFmod() {
    return 0;
}

int muteMusic(FMOD_BOOL mute) {
    return 0;
}

int updateSoundWorldPosition(int hashedId, float posVecX) {
    return 0;
}

int updateFmod() {
    return 0;
}

int loadSounds() {
    return 0;
}

int getLoadedSoundIndex(char* filePath) {
    return -1;
}

int getLoadedSoundsCount() {
    return 0;
}

const char* getLoadedSoundFilePath(int soundIndex) {
    return emptyString;
}

int loadMusicFilePath(char* filePath) {
    return -1;
}

int getMusic(char* filePath) {
    return -1;
}

int getMusicFilePathsCount() {
    return 0;
}

const char* getMusicFilePath(int musicIndex) {
    return emptyString;
}

int playSoundWorldPositional(int soundIndex, int hashedId, float posVecX) {
    return 0;
}

int playSoundWorld(int soundIndex, int hashedId) {
    return 0;
}

int playSound(int soundIndex) {
    return 0;
}

int fadeOutSound(int hashedId) {
    return 0;
}

int muteSound(int hashedId, FMOD_BOOL mute) {
    return 0;
}

int isMusicMenuPlaying() {
    return 0;
}

int isMusicWorldPlaying() {
    return 0;
}

int playOrResumeMusicMenu(int musicIndex) {
    return 0;
}

int playMusicWorld(int musicIndex) {
    return 0;
}

int fadeInMusicWorld(int musicIndex, float volumeMultiplier) {
    return 0;
}

int fadeInMusicWorldJukebox(int musicIndex, float jukeboxPosVecX) {
    return 0;
}

int setSoundVolume(float soundVolume) {
    return 0;
}

int setMusicVolume(float musicVolume) {
    return 0;
}

void setCameraWorldPosX(float cameraPosVecX) {
}

int rampMusicWorldToNormalVolume() {
    return 0;
}

int pauseMusicMenu(FMOD_BOOL paused) {
    return 0;
}

int pauseAudioWorld(FMOD_BOOL paused) {
    return 0;
}

int stopMusicWorld() {
    return 0;
}

int stopAudioWorld() {
    return 0;
}

int reloadLoadedSounds() {
    return 0;
}
