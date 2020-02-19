extern "C" {
#include "clips.h"
}
Environment* mainEnv = nullptr;
void setup() {
	Serial.begin(9600);
	mainEnv = CreateEnvironment();
	if (!mainEnv) {
		Serial.println("Unable to create CLIPS environment!");
		while(true);
	}
}

void loop() {
	CommandLoop(mainEnv);
}
