const REF_LEVEL_CODINGS = new Set(["dummy", "simple", "deviation"]);

const events = {
    update: function(ui) {
        updateVarOptions(ui);
    },

    onChange_vars: function(ui) {
        updateVarOptions(ui);
    },

    onChange_varOptions: function(ui) {
        updateVarOptions(ui);
    }
};

function supportsReferenceLevel(coding) {
    return REF_LEVEL_CODINGS.has(coding);
}

function normalizeVarOption(option, varName) {
    let coding = option && option.coding ? option.coding : "dummy";
    let ref = supportsReferenceLevel(coding) && option && option.ref ? option.ref : "";
    let standardize = option && option.standardize === true;

    return {
        var: varName,
        coding: coding,
        ref: ref,
        standardize: standardize
    };
}

function updateVarOptions(ui) {
    var varsVal = ui.vars.value();
    var varsList = Array.isArray(varsVal) ? [...varsVal] : [];

    var optsVal = ui.varOptions.value();
    var currentList = Array.isArray(optsVal) ? [...optsVal] : [];

    var newList = [];

    for (let i = 0; i < varsList.length; i++) {
        let varName = varsList[i];
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === varName) {
                found = currentList[j];
                break;
            }
        }

        newList.push(normalizeVarOption(found, varName));
    }

    if (JSON.stringify(currentList) !== JSON.stringify(newList))
        ui.varOptions.setValue(newList);

    updateLevelControls(ui);
}

function updateLevelControls(ui) {
    let dlist = ui.varOptions.value();

    ui.varOptions.applyToItems(0, (item, index, column) => {
        if (column === 2) {
            let row = dlist[index] || {};
            let enabled = supportsReferenceLevel(row.coding);

            item.setPropertyValue('variable', row.var);
            item.setPropertyValue('enable', enabled);

            if (item.input)
                item.input.disabled = !enabled;
        }
    });
}

module.exports = events;
