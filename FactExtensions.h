//
// Created by jwscoggins on 6/21/20.
//

#ifndef MAYA_FACTEXTENSIONS_H
#define MAYA_FACTEXTENSIONS_H

extern "C" {
    #include "Environment.h"
    #include "Fact.h"
}
#include <string>
#include "FunctionalExtensions.h"

namespace maya {

    class FactBuilder {
    public:
        FactBuilder(Environment& env, const std::string& name) : _env(env), _contents(CreateFactBuilder(&env, name.c_str())) { }
        ~FactBuilder() noexcept { ::FBDispose(_contents); }
        Fact* assert() noexcept { return FBAssert(_contents); }
        void abort() noexcept { FBAbort(_contents); }
        FactBuilderError setDeftemplate(const std::string& templateName) noexcept { return FBSetDeftemplate(_contents, templateName.c_str()); }
        PutSlotError putSlot(const std::string& slotName, CLIPSInteger* value) noexcept;
        PutSlotError putSlot(const std::string& slotName, CLIPSFloat* value) noexcept;
        PutSlotError putSlot(const std::string& slotName, CLIPSLexeme* value) noexcept;
        PutSlotError putSlot(const std::string& slotName, CLIPSExternalAddress * value) noexcept;
        PutSlotError putSlot(const std::string& slotName, CLIPSValue* value) noexcept;
#define X(kind, type) PutSlotError putSlot(const std::string& slotName, type value) noexcept { return FBPutSlot ## kind (_contents, slotName.c_str(), value); }
        X(Integer, int64_t);
        X(Float, double);
        X(Fact, Fact*);
        X(Instance, Instance*);
        X(Multifield, Multifield*);
#undef X
        inline FactBuilderError error() noexcept { return FBError(&_env); }
#define X(kind) \
    inline PutSlotError putSlot(const std::string& slotName, const std::string& value, TreatLexemeAs ## kind ) noexcept {  \
        return FBPutSlot ## kind (_contents, slotName.c_str(), value.c_str()); \
    }
        X(Symbol);
        X(String);
        X(InstanceName);
#undef X
    private:
        ::Environment& _env;
        ::FactBuilder* _contents = nullptr;

    };

} // end namespace maya


#endif //MAYA_FACTEXTENSIONS_H
