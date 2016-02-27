CC := cc
CXX := c++
OUTPUT := maya
PREFIX := /usr/local
LDFLAGS := -lm -lrt -lboost_system -lboost_filesystem
CFLAGS := -Os -g3 -std=c99
CXXFLAGS := -Os -g3 -std=c++11
COMMAND_PROMPT := "maya> "
BANNER_STRING := "\"     maya (based off of CLIPS \" VERSION_STRING \" \" CREATION_DATE_STRING \". Built on \" __DATE__ \" at \" __TIME__ \")\n\""

