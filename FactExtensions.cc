//
// Created by jwscoggins on 6/21/20.
//

#include "FactExtensions.h"
PutSlotError maya::FactBuilder::putSlot(const std::string &slotName, CLIPSInteger *value) noexcept { return FBPutSlotCLIPSInteger(_contents, slotName.c_str(), value); }
PutSlotError maya::FactBuilder::putSlot(const std::string &slotName, CLIPSFloat *value) noexcept { return FBPutSlotCLIPSFloat(_contents, slotName.c_str(), value); }
PutSlotError maya::FactBuilder::putSlot(const std::string &slotName, CLIPSLexeme *value) noexcept { return FBPutSlotCLIPSLexeme(_contents, slotName.c_str(), value); }
PutSlotError maya::FactBuilder::putSlot(const std::string &slotName, CLIPSExternalAddress *value) noexcept { return FBPutSlotCLIPSExternalAddress(_contents, slotName.c_str(), value); }
PutSlotError maya::FactBuilder::putSlot(const std::string &slotName, CLIPSValue *value) noexcept {
    return FBPutSlot(_contents, slotName.c_str(), value);
}
