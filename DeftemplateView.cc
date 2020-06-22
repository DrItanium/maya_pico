//
// Created by jwscoggins on 6/21/20.
//

#include "DeftemplateView.h"
namespace maya {
    FactView::Optional
    DeftemplateView::getFactList() noexcept {
        if (_raw.factList) {
            return FactView(*_raw.factList);
        } else {
            return std::nullopt;
        }
    }
    FactView::Optional
    DeftemplateView::getLastFact() noexcept {
        if (_raw.lastFact) {
            return FactView(*_raw.lastFact);
        } else {
            return std::nullopt;
        }
    }
}
