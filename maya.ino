extern "C" {
#include "clips.h"
}
Environment* mainEnv = nullptr;
void setup() {
	Serial.begin(9600);
	Serial.println("Booting up!");
	for (int i = 0; i < 1000; ++i) {
		digitalWrite(LED_BUILTIN, HIGH);
		delay(1000);
		digitalWrite(LED_BUILTIN, LOW);
		delay(1000);
	}
	delay(10000);
	mainEnv = CreateEnvironment();
	if (!mainEnv) {
		Serial.println("Unable to create CLIPS environment!");
		while(true) {
			digitalWrite(LED_BUILTIN, HIGH);
			delay(1000);
			digitalWrite(LED_BUILTIN, LOW);
			delay(1000);
		}
	} else {
		Serial.println("CLIPS Brought up!");
	}
}

void loop() {
	//CommandLoop(mainEnv);
}
