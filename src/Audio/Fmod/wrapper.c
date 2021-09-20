#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "fmod.h"
#include "fmod_studio.h"

#define MAX_BANK_EVENT_DESCRIPTION_PATH_SIZE 128
#define MAX_CHANNELS 64
#define MAX_SOUNDS 256
#define MAX_MUSIC_TIMESTAMPS 8
#define MAX_MUSICS 32
#define MUSIC_MENU_FADE_IN_SECS 5.0f
#define MUSIC_WORLD_FADE_OUT_SECS 2.0f
#define MUSIC_WORLD_FADE_IN_SECS 2.5f
#define MUSIC_WORLD_RAMP_TO_NORMAL_SECS 0.5f
#define MUSIC_WORLD_JUKEBOX_FADE_IN_SECS 1.0f
#define MIN_SOUND_PAN_VALUE -1.0f
#define MAX_SOUND_PAN_VALUE 1.0f
#define PAN_PARAM_NAME "pan"
#define PAN_OFFSET_MULTIPLIER 1.3f
#define VIRTUAL_RENDER_WIDTH 1920.0f  // should match Constants.hs virtualRenderWidth
#define NULL_HASHED_ID 0 // should match Id.hs hashId

#define MASTER_BUS_PATH "bus:/"
#define WORLD_BUS_PATH "bus:/World"
#define MASTER_STRINGS_BANK_PATH "data/sounds/Master.strings.bank"
#define MASTER_BANK_PATH "data/sounds/Master.bank"
#define UI_BANK_PATH "data/sounds/UI.bank"
#define PLAYER_BANK_PATH "data/sounds/Player.bank"
#define ENEMIES_BANK_PATH "data/sounds/Enemies.bank"
#define LEVEL_BANK_PATH "data/sounds/Level.bank"

#define TIMESTAMPS_DATA_FILE_PATH "data/music/timestamps.data"
#define MAX_TIMESTAMPS_FILE_PATH_SUBSTR 28
#define MAX_TIMESTAMPS_MILLISECONDS_COUNT 6
#define MAX_TIMESTAMPS_LINE 32

const char emptyString[] = "";

char* baseResourceDirPath;

typedef struct {
    char filePathSubstr[MAX_TIMESTAMPS_FILE_PATH_SUBSTR];
    unsigned int milliseconds[MAX_TIMESTAMPS_MILLISECONDS_COUNT];
    int millisecondsCount;
} MusicTimestamps;

typedef struct {
    FMOD_STUDIO_EVENTINSTANCE* eventInstance;
    int hashedId;
    int isPositional;
    float posVecX;
} PlayingWorldSound;

typedef struct {
    FMOD_SOUND* sound;
    char* filePath;
    char* absFilePath;
} MusicInfo;

typedef enum {
    PLAY_MUSIC_MENU_INACTIVE,
    PLAY_MUSIC_MENU_START,
    PLAY_MUSIC_MENU_ACTIVE,
} MusicMenuMode;

typedef enum {
    PLAY_MUSIC_WORLD_INACTIVE,
    PLAY_MUSIC_WORLD_START,
    PLAY_MUSIC_WORLD_FADE_IN,
    PLAY_MUSIC_WORLD_JUKEBOX_FADE_IN,
    PLAY_MUSIC_WORLD_ACTIVE,
    PLAY_MUSIC_WORLD_JUKEBOX_ACTIVE,
} MusicWorldMode;

FMOD_SYSTEM* fmodSystem;
FMOD_STUDIO_SYSTEM* fmodStudioSystem;

FMOD_STUDIO_BANK* masterBank;
FMOD_STUDIO_BANK* masterStringsBank;
FMOD_STUDIO_BANK* uiBank;
FMOD_STUDIO_BANK* playerBank;
FMOD_STUDIO_BANK* enemiesBank;
FMOD_STUDIO_BANK* levelBank;
FMOD_STUDIO_EVENTDESCRIPTION* bankEventDescriptions[MAX_SOUNDS];
PlayingWorldSound playingWorldSounds[MAX_CHANNELS];
int loadedSoundsCount;
int soundsPerFramePlayCount[MAX_SOUNDS];
float cameraWorldPosVecX;

MusicInfo musicInfos[MAX_MUSICS];
int musicInfosCount;
MusicTimestamps musicTimestamps[MAX_MUSIC_TIMESTAMPS];
int musicTimestampsCount;
MusicTimestamps* musicTimestamp;
MusicMenuMode musicMenuMode = PLAY_MUSIC_MENU_INACTIVE;
MusicWorldMode musicWorldMode = PLAY_MUSIC_WORLD_INACTIVE;
int musicMuted;
float musicWorldNextFadeInVolumeMultiplier = 1.0f;
float musicWorldJukeboxPosVecX = 0.0f;
FMOD_SOUND* musicMenuSound;
FMOD_SOUND* musicWorldSound;
FMOD_CHANNEL* musicMenuChannel;
FMOD_CHANNEL* musicWorldChannel;
FMOD_CHANNELGROUP* musicsChannelGroup;

int fadeOutMusicWorld();
int fadeOutSound(int hashedId);

int loadMusicTimestamps() {
    char* absFilePath = malloc(strlen(baseResourceDirPath) + strlen(TIMESTAMPS_DATA_FILE_PATH) + 1);
    strcpy(absFilePath, baseResourceDirPath);
    strcat(absFilePath, TIMESTAMPS_DATA_FILE_PATH);
    FILE* file = fopen(absFilePath, "r");
    free(absFilePath);

    if (file == NULL)
        return -1;

    char line[MAX_TIMESTAMPS_LINE];
    while (fgets(line, sizeof(line), file) != NULL) {
        if (line[0] == '\n' || line[0] == '\r')
            continue;
        else if (isspace(line[0])) {
            int musicTimestampsIndex = musicTimestampsCount - 1;
            if (musicTimestampsIndex < 0) {
                fclose(file);
                return -1;
            }

            int milliseconds;
            if (sscanf(line, " - %d", &milliseconds) < 1) {
                fclose(file);
                return -1;
            }

            int* millisecondsCount = &musicTimestamps[musicTimestampsIndex].millisecondsCount;
            musicTimestamps[musicTimestampsIndex].milliseconds[(*millisecondsCount)++] = milliseconds;
        }
        else {
            char* filePathSubstr = musicTimestamps[musicTimestampsCount++].filePathSubstr;
            strcpy(filePathSubstr, line);

            int colonIndex = strlen(filePathSubstr) - 2;
            if (colonIndex <= 0 || filePathSubstr[colonIndex] != ':') {
                fclose(file);
                return -1;
            }
            filePathSubstr[colonIndex] = '\0';
        }
    }
    fclose(file);

    return 0;
}

int initFmod(char* baseResourceDir) {
    baseResourceDirPath = malloc(strlen(baseResourceDir) + 1);
    strcpy(baseResourceDirPath, baseResourceDir);

    if (FMOD_Studio_System_Create(&fmodStudioSystem, FMOD_VERSION) != FMOD_OK)
        return -1;
    if (FMOD_Studio_System_GetCoreSystem(fmodStudioSystem, &fmodSystem) != FMOD_OK)
        return -1;

    if (
        FMOD_Studio_System_Initialize(
            fmodStudioSystem,
            MAX_CHANNELS,
            FMOD_STUDIO_INIT_NORMAL,
            FMOD_INIT_NORMAL,
            NULL
        ) != FMOD_OK
    ) {
        return -1;
    }

    if (FMOD_System_CreateChannelGroup(fmodSystem, 0, &musicsChannelGroup) != FMOD_OK)
        return -1;

    if (loadMusicTimestamps() != 0)
        return -1;

    return 0;
}

int freeBank(FMOD_STUDIO_BANK** bank) {
    if (*bank == NULL)
        return 0;

    if (FMOD_Studio_Bank_Unload(*bank) != FMOD_OK)
        return -1;
    *bank = NULL;

    return 0;
}

int freeSounds() {
    int ret = 0;

    if (freeBank(&levelBank) != 0)
        ret = -1;
    if (freeBank(&enemiesBank) != 0)
        ret = -1;
    if (freeBank(&playerBank) != 0)
        ret = -1;
    if (freeBank(&uiBank) != 0)
        ret = -1;
    if (freeBank(&masterStringsBank) != 0)
        ret = -1;
    if (freeBank(&masterBank) != 0)
        ret = -1;

    for (int i = 0; i < loadedSoundsCount; ++i) {
        playingWorldSounds[i].eventInstance = NULL;
    }
    loadedSoundsCount = 0;

    return ret;
}

int freeFmod() {
    int ret = 0;

    if (FMOD_Studio_System_Release(fmodStudioSystem) != FMOD_OK)
        ret = -1;

    if (freeSounds() != 0)
        ret = -1;

    for (int i = 0; i < musicInfosCount; ++i) {
        free(musicInfos[i].absFilePath);
        free(musicInfos[i].filePath);
    }
    musicInfosCount = 0;

    free(baseResourceDirPath);

    return 0;
}

int getMusicChannelPosition(unsigned int* position) {
    if (!musicWorldChannel)
        return -1;

    if (FMOD_Channel_GetPosition(musicWorldChannel, position, FMOD_TIMEUNIT_MS) != FMOD_OK)
        return -1;

    return 0;
}

void clearMusicWorld() {
    musicWorldChannel = (FMOD_CHANNEL*)0;
    musicWorldSound = (FMOD_SOUND*)0;
    musicWorldMode = PLAY_MUSIC_WORLD_INACTIVE;
}

int muteMusic(FMOD_BOOL mute) {
    musicMuted = mute;

    if (FMOD_ChannelGroup_SetMute(musicsChannelGroup, mute) != FMOD_OK)
        return -1;

    return 0;
}

float calculateSoundPan(float posVecX) {
    float halfRenderWidth = VIRTUAL_RENDER_WIDTH / 2.0;
    float cameraCenterX = cameraWorldPosVecX + halfRenderWidth;
    float diffX = posVecX - cameraCenterX;
    float pan = diffX / (halfRenderWidth * PAN_OFFSET_MULTIPLIER);

    if (pan < MIN_SOUND_PAN_VALUE)
        return MIN_SOUND_PAN_VALUE;
    else if (pan > MAX_SOUND_PAN_VALUE)
        return MAX_SOUND_PAN_VALUE;
    else
        return pan;
}

int updateSoundsWorld() {
    memset(soundsPerFramePlayCount, 0, sizeof(soundsPerFramePlayCount));

    int ret = 0;
    for (int i = 0; i < MAX_CHANNELS; ++i) {
        FMOD_STUDIO_EVENTINSTANCE* eventInstance = playingWorldSounds[i].eventInstance;
        if (eventInstance != NULL && playingWorldSounds[i].isPositional) {
            float panValue = calculateSoundPan(playingWorldSounds[i].posVecX);
            if (FMOD_Studio_EventInstance_SetParameterByName(eventInstance, PAN_PARAM_NAME, panValue, 1) != FMOD_OK)
                ret = -1;
        }
    }

    return ret;
}

int updateSoundWorldPosition(int hashedId, float posVecX) {
    for (int i = 0; i < MAX_CHANNELS; ++i) {
        if (playingWorldSounds[i].eventInstance != NULL && playingWorldSounds[i].hashedId == hashedId) {
            playingWorldSounds[i].posVecX = posVecX;
            return 0;
        }
    }
    return -1;
}

int updateMusicMenu() {
    if (!musicMenuSound)
        return 0;

    FMOD_OPENSTATE openState;
    if (FMOD_Sound_GetOpenState(musicMenuSound, &openState, 0, 0, 0) != FMOD_OK)
        return 0;

    switch (musicMenuMode) {
        case PLAY_MUSIC_MENU_INACTIVE:
            break;

        case PLAY_MUSIC_MENU_ACTIVE:
            break;

        case PLAY_MUSIC_MENU_START:
            if (openState == FMOD_OPENSTATE_READY) {
                if (
                    FMOD_System_PlaySound(
                        fmodSystem,
                        musicMenuSound,
                        musicsChannelGroup,
                        0,
                        &musicMenuChannel
                    ) != FMOD_OK
                ) {
                    return -1;
                }

                if (musicMuted)
                    muteMusic(musicMuted);

                musicMenuMode = PLAY_MUSIC_MENU_ACTIVE;
            }
            break;
    }

    return 0;
}

int playMusicWorldStart() {
    FMOD_OPENSTATE openState;
    if (FMOD_Sound_GetOpenState(musicWorldSound, &openState, 0, 0, 0) != FMOD_OK)
        return -1;
    if (openState != FMOD_OPENSTATE_READY)
        return 0;

    if (
        FMOD_System_PlaySound(
            fmodSystem,
            musicWorldSound,
            musicsChannelGroup,
            0,
            &musicWorldChannel
        ) != FMOD_OK
    ) {
        return -1;
    }

    if (FMOD_Channel_SetPan(musicWorldChannel, 0.0f) != FMOD_OK)
        return -1;

    if (musicMuted)
        muteMusic(musicMuted);

    musicWorldMode = PLAY_MUSIC_WORLD_ACTIVE;

    return 0;
}

int getDspClockAndRate(FMOD_CHANNEL* channel, unsigned long long* dspClock, int* rate) {
    if (FMOD_System_GetSoftwareFormat(fmodSystem, rate, 0, 0) != FMOD_OK)
        return -1;

    if (FMOD_Channel_GetDSPClock(channel, 0, dspClock) != FMOD_OK)
        return -1;

    return 0;
}

int playMusicWorldFadeIn() {
    FMOD_OPENSTATE openState;
    if (FMOD_Sound_GetOpenState(musicWorldSound, &openState, 0, 0, 0) != FMOD_OK)
        return -1;
    if (openState != FMOD_OPENSTATE_READY)
        return 0;

    // ignore any errors reading currently playing music position
    unsigned int position = 0;
    if (getMusicChannelPosition(&position) == 0) {
        if (musicTimestamp != NULL) {
            for (int i = 0; i < musicTimestamp->millisecondsCount; ++i) {
                unsigned int ms = musicTimestamp->milliseconds[i];
                if (position < ms) {
                    position = ms;
                    break;
                }
            }
        }
    }

    // ignore any errors fading out current music
    fadeOutMusicWorld();

    if (
        FMOD_System_PlaySound(
            fmodSystem,
            musicWorldSound,
            musicsChannelGroup,
            1,
            &musicWorldChannel
        ) != FMOD_OK
    ) {
        return -1;
    }

    if (FMOD_Channel_SetPosition(musicWorldChannel, position, FMOD_TIMEUNIT_MS) != FMOD_OK)
        return -1;

    unsigned long long dspClock;
    int rate;
    if (getDspClockAndRate(musicWorldChannel, &dspClock, &rate) != 0)
        return -1;
    unsigned long long offset = rate * MUSIC_WORLD_FADE_IN_SECS;

    FMOD_Channel_RemoveFadePoints(musicWorldChannel, dspClock, dspClock + offset);

    if (FMOD_Channel_AddFadePoint(musicWorldChannel, dspClock, 0.0f) != FMOD_OK)
        return -1;

    if (
        FMOD_Channel_AddFadePoint(
            musicWorldChannel,
            dspClock + offset,
            musicWorldNextFadeInVolumeMultiplier
        ) != FMOD_OK
    ) {
        return -1;
    }

    if (FMOD_Channel_SetPan(musicWorldChannel, 0.0f) != FMOD_OK)
        return -1;

    if (FMOD_Channel_SetPaused(musicWorldChannel, 0) != FMOD_OK)
        return -1;

    if (musicMuted)
        muteMusic(musicMuted);

    musicWorldMode = PLAY_MUSIC_WORLD_ACTIVE;

    return 0;
}

int playMusicWorldJukeboxFadeIn() {
    FMOD_OPENSTATE openState;
    if (FMOD_Sound_GetOpenState(musicWorldSound, &openState, 0, 0, 0) != FMOD_OK)
        return -1;
    if (openState != FMOD_OPENSTATE_READY)
        return 0;

    // ignore any errors fading out current music
    fadeOutMusicWorld();

    if (
        FMOD_System_PlaySound(
            fmodSystem,
            musicWorldSound,
            musicsChannelGroup,
            1,
            &musicWorldChannel
        ) != FMOD_OK
    ) {
        return -1;
    }

    if (FMOD_Channel_SetPosition(musicWorldChannel, 0, FMOD_TIMEUNIT_MS) != FMOD_OK)
        return -1;

    unsigned long long dspClock;
    int rate;
    if (getDspClockAndRate(musicWorldChannel, &dspClock, &rate) != 0)
        return -1;
    unsigned long long offset = rate * MUSIC_WORLD_JUKEBOX_FADE_IN_SECS;

    FMOD_Channel_RemoveFadePoints(musicWorldChannel, dspClock, dspClock + offset);

    if (FMOD_Channel_AddFadePoint(musicWorldChannel, dspClock, 0.0f) != FMOD_OK)
        return -1;

    if (
        FMOD_Channel_AddFadePoint(
            musicWorldChannel,
            dspClock + offset,
            musicWorldNextFadeInVolumeMultiplier
        ) != FMOD_OK
    ) {
        return -1;
    }

    float pan = calculateSoundPan(musicWorldJukeboxPosVecX);
    if (FMOD_Channel_SetPan(musicWorldChannel, pan) != FMOD_OK)
        return -1;

    if (FMOD_Channel_SetPaused(musicWorldChannel, 0) != FMOD_OK)
        return -1;

    if (musicMuted)
        muteMusic(musicMuted);

    musicWorldMode = PLAY_MUSIC_WORLD_JUKEBOX_ACTIVE;

    return 0;
}

int updateMusicWorld() {
    if (!musicWorldSound) {
        return 0;
    }

    switch (musicWorldMode) {
        case PLAY_MUSIC_WORLD_INACTIVE:
        case PLAY_MUSIC_WORLD_ACTIVE:
            break;

        case PLAY_MUSIC_WORLD_JUKEBOX_ACTIVE:
            {
                float pan = calculateSoundPan(musicWorldJukeboxPosVecX);
                if (FMOD_Channel_SetPan(musicWorldChannel, pan) != FMOD_OK)
                    return -1;
            }
            break;

        case PLAY_MUSIC_WORLD_START:
            playMusicWorldStart();
            break;

        case PLAY_MUSIC_WORLD_FADE_IN:
            playMusicWorldFadeIn();
            break;

        case PLAY_MUSIC_WORLD_JUKEBOX_FADE_IN:
            playMusicWorldJukeboxFadeIn();
            break;
    }

    return 0;
}

int updateFmod() {
    int ret = 0;

    if (FMOD_Studio_System_Update(fmodStudioSystem) != FMOD_OK)
        return -1;

    if (updateSoundsWorld() != 0)
        ret = -1;
    if (updateMusicMenu() != 0)
        ret = -1;
    if (updateMusicWorld() != 0)
        ret = -1;

    return ret;
}

int loadFmodStudioMasterStringsBank() {
    char* absFilePath = malloc(strlen(baseResourceDirPath) + strlen(MASTER_STRINGS_BANK_PATH) + 1);
    strcpy(absFilePath, baseResourceDirPath);
    strcat(absFilePath, MASTER_STRINGS_BANK_PATH);

    if (
        FMOD_Studio_System_LoadBankFile(
            fmodStudioSystem,
            absFilePath,
            FMOD_STUDIO_LOAD_BANK_NORMAL,
            &masterStringsBank
        ) != FMOD_OK
       ) {
        free(absFilePath);
        return -1;
    }
    free(absFilePath);

    return 0;
}

int loadFmodStudioBank(char* filePath, FMOD_STUDIO_BANK** bank) {
    if (loadedSoundsCount >= MAX_SOUNDS)
        return -1;

    char* absFilePath = malloc(strlen(baseResourceDirPath) + strlen(filePath) + 1);
    strcpy(absFilePath, baseResourceDirPath);
    strcat(absFilePath, filePath);

    if (
        FMOD_Studio_System_LoadBankFile(
            fmodStudioSystem,
            absFilePath,
            FMOD_STUDIO_LOAD_BANK_NORMAL,
            bank
        ) != FMOD_OK
    ) {
        free(absFilePath);
        return -1;
    }
    free(absFilePath);

    int count;
    if (
        FMOD_Studio_Bank_GetEventList(
            *bank,
            &bankEventDescriptions[loadedSoundsCount],
            MAX_SOUNDS - loadedSoundsCount,
            &count
        ) != FMOD_OK
    ) {
        return -1;
    }
    loadedSoundsCount += count;

    if (FMOD_Studio_Bank_LoadSampleData(*bank) != FMOD_OK)
        return -1;

    return 0;
}

int loadSounds() {
    int ret = 0;

    if (loadFmodStudioMasterStringsBank() != 0)
        ret = -1;
    if (loadFmodStudioBank(MASTER_BANK_PATH, &masterBank) != 0)
        ret = -1;
    if (loadFmodStudioBank(UI_BANK_PATH, &uiBank) != 0)
        ret = -1;
    if (loadFmodStudioBank(PLAYER_BANK_PATH, &playerBank) != 0)
        ret = -1;
    if (loadFmodStudioBank(ENEMIES_BANK_PATH, &enemiesBank) != 0)
        ret = -1;
    if (loadFmodStudioBank(LEVEL_BANK_PATH, &levelBank) != 0)
        ret = -1;

    return ret;
}

int getLoadedSoundIndex(char* filePath) {
    static char path[MAX_BANK_EVENT_DESCRIPTION_PATH_SIZE];

    for (int i = 0; i < loadedSoundsCount; ++i) {
        if (
            FMOD_Studio_EventDescription_GetPath(
                bankEventDescriptions[i],
                path,
                MAX_BANK_EVENT_DESCRIPTION_PATH_SIZE,
                NULL
            ) == FMOD_OK && strcmp(path, filePath) == 0
        ) {
            return i;
        }
    }
    return -1;
}

int getLoadedSoundsCount() {
    return loadedSoundsCount;
}

// Only call from Audio/Fmod.hs FFI! This assumes the char* value will get copied to a CString!
const char* getLoadedSoundFilePath(int soundIndex) {
    static char path[MAX_BANK_EVENT_DESCRIPTION_PATH_SIZE];

    if (soundIndex < 0 || soundIndex >= loadedSoundsCount)
        return emptyString;

    if (
        FMOD_Studio_EventDescription_GetPath(
            bankEventDescriptions[soundIndex],
            path,
            MAX_BANK_EVENT_DESCRIPTION_PATH_SIZE,
            NULL
        ) != FMOD_OK
    ) {
        return emptyString;
    }

    return path;
}

int loadMusicFilePath(char* filePath) {
    if (musicInfosCount >= MAX_MUSICS)
        return -1;

    int musicIndex = musicInfosCount;
    musicInfos[musicIndex].filePath = malloc(strlen(filePath) + 1);
    strcpy(musicInfos[musicIndex].filePath, filePath);
    musicInfos[musicIndex].absFilePath = malloc(strlen(baseResourceDirPath) + strlen(filePath) + 1);
    strcpy(musicInfos[musicIndex].absFilePath, baseResourceDirPath);
    strcat(musicInfos[musicIndex].absFilePath, filePath);
    ++musicInfosCount;

    return musicIndex;
}

int getMusic(char* filePath) {
    for (int i = 0; i < musicInfosCount; ++i) {
        if (strcmp(musicInfos[i].filePath, filePath) == 0) {
            return i;
        }
    }
    return -1;
}

int getMusicFilePathsCount() {
    return musicInfosCount + 1;
}

const char* getMusicFilePath(int musicIndex) {
    if (musicIndex < 0 || musicIndex >= musicInfosCount)
        return emptyString;

    return musicInfos[musicIndex].filePath;
}

FMOD_RESULT F_CALLBACK worldEventInstanceCallback(
    FMOD_STUDIO_EVENT_CALLBACK_TYPE callbackType,
    FMOD_STUDIO_EVENTINSTANCE* eventInstance,
    void *parameters
) {
    void* playingWorldSound = NULL;
    if (FMOD_Studio_EventInstance_GetUserData(eventInstance, &playingWorldSound) == FMOD_OK)
        ((PlayingWorldSound*)playingWorldSound)->eventInstance = NULL;
    return FMOD_OK;
}

PlayingWorldSound* getFreePlayingWorldSound() {
    for (int i = 0; i < MAX_CHANNELS; ++i) {
        if (playingWorldSounds[i].eventInstance == NULL)
            return &playingWorldSounds[i];
    }
    return NULL;
}

PlayingWorldSound* getPlayingWorldSound(int hashedId) {
    if (hashedId == NULL_HASHED_ID)
        return NULL;

    for (int i = 0; i < MAX_CHANNELS; ++i) {
        if (playingWorldSounds[i].eventInstance != NULL && playingWorldSounds[i].hashedId == hashedId)
            return &playingWorldSounds[i];
    }
    return NULL;
}

int playSoundWorldPositional(int soundIndex, int hashedId, float posVecX) {
    if (soundIndex < 0 || soundIndex >= loadedSoundsCount)
        return -1;
    if (getPlayingWorldSound(hashedId) != NULL) {
        return -1;
    }

    FMOD_STUDIO_EVENTINSTANCE* eventInstance;
    if (FMOD_Studio_EventDescription_CreateInstance(bankEventDescriptions[soundIndex], &eventInstance) != FMOD_OK)
        return -1;
    float panValue = calculateSoundPan(posVecX);
    if (FMOD_Studio_EventInstance_SetParameterByName(eventInstance, PAN_PARAM_NAME, panValue, 1) != FMOD_OK)
        return -1;
    if (FMOD_Studio_EventInstance_Start(eventInstance) != FMOD_OK)
        return -1;
    if (FMOD_Studio_EventInstance_Release(eventInstance) != FMOD_OK)
        return -1;

    PlayingWorldSound* playingWorldSound = getFreePlayingWorldSound();
    if (playingWorldSound != NULL) {
        playingWorldSound->eventInstance = eventInstance;
        playingWorldSound->hashedId = hashedId;
        playingWorldSound->isPositional = 1;
        playingWorldSound->posVecX = posVecX;

        if (FMOD_Studio_EventInstance_SetUserData(eventInstance, (void*)playingWorldSound) != FMOD_OK)
            return -1;
        if (
            FMOD_Studio_EventInstance_SetCallback(
                eventInstance,
                worldEventInstanceCallback,
                FMOD_STUDIO_EVENT_CALLBACK_STOPPED
            ) != FMOD_OK
        ) {
            return -1;
        }

        // scale volume for multiple concurrent same sounds
        int perFramePlayCount = soundsPerFramePlayCount[soundIndex];
        if (perFramePlayCount > 0) {
            float volume = 1.0 / (2.0 * perFramePlayCount);
            if (FMOD_Studio_EventInstance_SetVolume(playingWorldSound->eventInstance, volume) != FMOD_OK)
                return -1;
        }
        ++soundsPerFramePlayCount[soundIndex];
    }

    return 0;
}

int playSoundWorld(int soundIndex, int hashedId) {
    if (soundIndex < 0 || soundIndex >= loadedSoundsCount)
        return -1;
    if (getPlayingWorldSound(hashedId) != NULL)
        return -1;

    FMOD_STUDIO_EVENTINSTANCE* eventInstance;
    if (FMOD_Studio_EventDescription_CreateInstance(bankEventDescriptions[soundIndex], &eventInstance) != FMOD_OK)
        return -1;
    if (FMOD_Studio_EventInstance_Start(eventInstance) != FMOD_OK)
        return -1;
    if (FMOD_Studio_EventInstance_Release(eventInstance) != FMOD_OK)
        return -1;

    PlayingWorldSound* playingWorldSound = getFreePlayingWorldSound();
    if (playingWorldSound != NULL) {
        playingWorldSound->eventInstance = eventInstance;
        playingWorldSound->hashedId = hashedId;
        playingWorldSound->isPositional = 0;
        playingWorldSound->posVecX = 0.0f;

        if (FMOD_Studio_EventInstance_SetUserData(eventInstance, (void*)playingWorldSound) != FMOD_OK)
            return -1;
        if (
            FMOD_Studio_EventInstance_SetCallback(
                eventInstance,
                worldEventInstanceCallback,
                FMOD_STUDIO_EVENT_CALLBACK_STOPPED
            ) != FMOD_OK
        ) {
            return -1;
        }
    }

    return 0;
}

int playSound(int soundIndex) {
    if (soundIndex < 0 || soundIndex >= loadedSoundsCount)
        return -1;

    FMOD_STUDIO_EVENTINSTANCE* eventInstance;
    if (FMOD_Studio_EventDescription_CreateInstance(bankEventDescriptions[soundIndex], &eventInstance) != FMOD_OK)
        return -1;
    if (FMOD_Studio_EventInstance_Start(eventInstance) != FMOD_OK)
        return -1;
    if (FMOD_Studio_EventInstance_Release(eventInstance) != FMOD_OK)
        return -1;

    return 0;
}

int stopSound(int hashedId, FMOD_STUDIO_STOP_MODE mode) {
    PlayingWorldSound* playingWorldSound = getPlayingWorldSound(hashedId);
    if (playingWorldSound == NULL)
        return 0;

    FMOD_STUDIO_PLAYBACK_STATE playbackState = FMOD_STUDIO_PLAYBACK_PLAYING;
    if (FMOD_Studio_EventInstance_GetPlaybackState(playingWorldSound->eventInstance, &playbackState) == FMOD_OK) {
        if (playbackState == FMOD_STUDIO_PLAYBACK_STOPPING || playbackState == FMOD_STUDIO_PLAYBACK_STOPPED)
            return 0;
    }
    else
        return -1;

    if (FMOD_Studio_EventInstance_Stop(playingWorldSound->eventInstance, mode) != FMOD_OK)
        return -1;

    return 0;
}

int fadeOutSound(int hashedId) {
    return stopSound(hashedId, FMOD_STUDIO_STOP_ALLOWFADEOUT);
}

int muteSound(int hashedId, FMOD_BOOL mute) {
    PlayingWorldSound* playingWorldSound = getPlayingWorldSound(hashedId);
    if (playingWorldSound != NULL) {
        float volume = mute ? 0.0f : 1.0f;
        if (FMOD_Studio_EventInstance_SetVolume(playingWorldSound->eventInstance, volume) != FMOD_OK)
            return 0;
    }

    return 0;
}

int isMusicMenuPlaying() {
    if (!musicMenuChannel)
        return 0;

    FMOD_BOOL isPlaying;
    if (FMOD_Channel_IsPlaying(musicMenuChannel, &isPlaying) != FMOD_OK)
        return 0;

    FMOD_BOOL paused;
    if (FMOD_Channel_GetPaused(musicMenuChannel, &paused) != FMOD_OK)
        return 0;

    return isPlaying && !paused;
}

int isMusicWorldPlaying() {
    if (!musicWorldChannel)
        return 0;

    FMOD_BOOL isPlaying;
    if (FMOD_Channel_IsPlaying(musicWorldChannel, &isPlaying) != FMOD_OK)
        return 0;

    FMOD_BOOL paused;
    if (FMOD_Channel_GetPaused(musicWorldChannel, &paused) != FMOD_OK)
        return 0;

    return isPlaying && !paused;
}

int playMusicMenu(int musicIndex) {
    if (musicIndex < 0 || musicIndex >= musicInfosCount)
        return -1;

    if (isMusicWorldPlaying())
        FMOD_Channel_SetPaused(musicWorldChannel, 1);

    if (
        FMOD_System_CreateStream(
            fmodSystem,
            musicInfos[musicIndex].absFilePath,
            FMOD_LOOP_NORMAL | FMOD_NONBLOCKING,
            0,
            &musicInfos[musicIndex].sound
        ) != FMOD_OK
    )
        return -1;

    musicMenuSound = musicInfos[musicIndex].sound;
    musicMenuMode = PLAY_MUSIC_MENU_START;

    return 0;
}

int resumeMusicMenu() {
    if (!musicMenuChannel)
        return -1;

    if (isMusicWorldPlaying())
        FMOD_Channel_SetPaused(musicWorldChannel, 1);

    unsigned long long dspClock;
    int rate;
    if (getDspClockAndRate(musicWorldChannel, &dspClock, &rate) != 0)
        return -1;
    unsigned long long offset = rate * MUSIC_MENU_FADE_IN_SECS;

    FMOD_Channel_RemoveFadePoints(musicMenuChannel, dspClock, dspClock + offset);

    if (FMOD_Channel_AddFadePoint(musicMenuChannel, dspClock, 0.0f) != FMOD_OK)
        return -1;
    if (FMOD_Channel_AddFadePoint(musicMenuChannel, dspClock + offset, 1.0) != FMOD_OK)
        return -1;

    if (FMOD_Channel_SetPaused(musicMenuChannel, 0) != FMOD_OK)
        return -1;

    return 0;
}

int playOrResumeMusicMenu(int musicIndex) {
    if (isMusicMenuPlaying() || musicMenuMode == PLAY_MUSIC_MENU_START)
        return 0;

    if (musicMenuChannel) {
        if (resumeMusicMenu() != 0)
            return -1;
    }
    else {
        if (playMusicMenu(musicIndex) != 0)
            return -1;
    }

    return 0;
}

int playMusicWorld(int musicIndex) {
    if (musicIndex < 0 || musicIndex >= musicInfosCount)
        return -1;

    if (isMusicMenuPlaying())
        FMOD_Channel_SetPaused(musicMenuChannel, 1);

    if (isMusicWorldPlaying())
        FMOD_Channel_Stop(musicWorldChannel);

    if (musicInfos[musicIndex].sound) {
        FMOD_Sound_Release(musicInfos[musicIndex].sound);
        musicInfos[musicIndex].sound = NULL;
    }

    if (
        FMOD_System_CreateStream(
            fmodSystem,
            musicInfos[musicIndex].absFilePath,
            FMOD_LOOP_NORMAL | FMOD_NONBLOCKING,
            0,
            &musicInfos[musicIndex].sound
        ) != FMOD_OK
    ) {
        return -1;
    }

    musicWorldSound = musicInfos[musicIndex].sound;
    musicWorldMode = PLAY_MUSIC_WORLD_START;

    return 0;
}

int fadeOutMusicWorld() {
    if (!musicWorldChannel)
        return 0;

    unsigned long long dspClock;
    int rate;
    if (getDspClockAndRate(musicWorldChannel, &dspClock, &rate) != 0)
        return -1;
    unsigned long long offset = rate * MUSIC_WORLD_FADE_OUT_SECS;

    FMOD_Channel_RemoveFadePoints(musicWorldChannel, dspClock, dspClock + offset);

    if (FMOD_Channel_AddFadePoint(musicWorldChannel, dspClock, 1.0f) != FMOD_OK)
        return -1;
    if (FMOD_Channel_AddFadePoint(musicWorldChannel, dspClock + offset, 0.0f) != FMOD_OK)
        return -1;
    if (FMOD_Channel_SetDelay(musicWorldChannel, 0, dspClock + offset, 1) != FMOD_OK)
        return -1;

    return 0;
}

int fadeInMusicWorld(int musicIndex, float volumeMultiplier) {
    if (musicIndex < 0 || musicIndex >= musicInfosCount)
        return -1;

    if (isMusicMenuPlaying())
        FMOD_Channel_SetPaused(musicMenuChannel, 1);

    if (musicInfos[musicIndex].sound) {
        FMOD_Sound_Release(musicInfos[musicIndex].sound);
        musicInfos[musicIndex].sound = NULL;
    }

    if (
        FMOD_System_CreateStream(
            fmodSystem,
            musicInfos[musicIndex].absFilePath,
            FMOD_LOOP_NORMAL | FMOD_NONBLOCKING,
            0,
            &musicInfos[musicIndex].sound
        ) != FMOD_OK
    )
        return -1;

    musicWorldSound = musicInfos[musicIndex].sound;
    musicWorldMode = PLAY_MUSIC_WORLD_FADE_IN;
    musicWorldNextFadeInVolumeMultiplier = volumeMultiplier;

    musicTimestamp = NULL;
    for (int i = 0; i < musicTimestampsCount; ++i) {
        if (strstr(musicInfos[musicIndex].filePath, musicTimestamps[i].filePathSubstr) != NULL) {
            musicTimestamp = &musicTimestamps[i];
            break;
        }
    }

    return 0;
}

int fadeInMusicWorldJukebox(int musicIndex, float jukeboxPosVecX) {
    if (musicIndex < 0 || musicIndex >= musicInfosCount)
        return -1;

    if (isMusicMenuPlaying())
        FMOD_Channel_SetPaused(musicMenuChannel, 1);

    if (musicInfos[musicIndex].sound) {
        FMOD_Sound_Release(musicInfos[musicIndex].sound);
        musicInfos[musicIndex].sound = NULL;
    }

    if (
        FMOD_System_CreateStream(
            fmodSystem,
            musicInfos[musicIndex].absFilePath,
            FMOD_LOOP_NORMAL | FMOD_NONBLOCKING,
            0,
            &musicInfos[musicIndex].sound
        ) != FMOD_OK
    )
        return -1;

    musicWorldSound = musicInfos[musicIndex].sound;
    musicWorldMode = PLAY_MUSIC_WORLD_JUKEBOX_FADE_IN;
    musicWorldNextFadeInVolumeMultiplier = 1.0f;
    musicWorldJukeboxPosVecX = jukeboxPosVecX;

    return 0;
}

int setSoundVolume(float soundVolume) {
    FMOD_STUDIO_BUS* masterBus;
    if (FMOD_Studio_System_GetBus(fmodStudioSystem, MASTER_BUS_PATH, &masterBus) != FMOD_OK)
        return -1;
    if (FMOD_Studio_Bus_SetVolume(masterBus, soundVolume) != FMOD_OK)
        return -1;

    return 0;
}

int setMusicVolume(float musicVolume) {
    if (FMOD_ChannelGroup_SetVolume(musicsChannelGroup, musicVolume) != FMOD_OK)
        return -1;

    return 0;
}

void setCameraWorldPosX(float cameraPosVecX) {
    cameraWorldPosVecX = cameraPosVecX;
}

int rampMusicWorldToNormalVolume() {
    if (!musicWorldChannel)
        return 0;

    unsigned long long dspClock;
    int rate;
    if (getDspClockAndRate(musicWorldChannel, &dspClock, &rate) != 0)
        return -1;

    FMOD_Channel_RemoveFadePoints(musicWorldChannel, dspClock, dspClock + rate * MUSIC_WORLD_FADE_IN_SECS);

    if (FMOD_Channel_AddFadePoint(musicWorldChannel, dspClock, musicWorldNextFadeInVolumeMultiplier) != FMOD_OK)
        return -1;
    if (
        FMOD_Channel_AddFadePoint(
            musicWorldChannel,
            dspClock + rate * MUSIC_WORLD_RAMP_TO_NORMAL_SECS,
            1.0
        ) != FMOD_OK
    )
        return -1;

    return 0;
}

int pauseMusicMenu(FMOD_BOOL paused) {
    if (musicMenuChannel) {
        if (FMOD_Channel_SetPaused(musicMenuChannel, paused) != FMOD_OK)
            return -1;
    }

    return 0;
}

int pauseAudioWorld(FMOD_BOOL paused) {
    int ret = 0;

    FMOD_STUDIO_BUS* worldBus;
    if (FMOD_Studio_System_GetBus(fmodStudioSystem, WORLD_BUS_PATH, &worldBus) != FMOD_OK)
        ret = -1;
    if (FMOD_Studio_Bus_SetPaused(worldBus, paused) != FMOD_OK)
        ret = -1;

    if (musicWorldChannel) {
        if (FMOD_Channel_SetPaused(musicWorldChannel, paused) != FMOD_OK)
            ret = -1;
    }

    return ret;
}

int stopSoundsWorld() {
    FMOD_STUDIO_BUS* worldBus;
    if (FMOD_Studio_System_GetBus(fmodStudioSystem, WORLD_BUS_PATH, &worldBus) != FMOD_OK)
        return -1;
    if (FMOD_Studio_Bus_StopAllEvents(worldBus, FMOD_STUDIO_STOP_IMMEDIATE) != FMOD_OK)
        return -1;

    for (int i = 0; i < MAX_CHANNELS; ++i) {
        playingWorldSounds[i].eventInstance = NULL;
    }

    return 0;
}

int stopMusicWorld() {
    int ret = 0;

    if (musicWorldChannel) {
        if (FMOD_Channel_Stop(musicWorldChannel) != FMOD_OK)
            ret = -1;
    }
    clearMusicWorld();

    return 0;
}

int stopAudioWorld() {
    int ret = 0;

    if (stopSoundsWorld() != 0)
        ret = -1;
    if (stopMusicWorld() != 0)
        ret = -1;

    return ret;
}

int reloadLoadedSounds() {
    int ret = 0;

    if (stopSoundsWorld() != 0)
        ret = -1;
    if (freeSounds() != 0)
        ret = -1;
    if (loadSounds() != 0)
        ret = -1;

    return ret;
}
