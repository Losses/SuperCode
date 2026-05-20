const events = {
    update: function(ui) {
        updateVarOptions(ui, this);
    },

    onChange_vars: function(ui) {
        updateVarOptions(ui, this);
    }
};

function updateVarOptions(ui, context) {
    var varsVal = ui.vars.value();
    var varsList = Array.isArray(varsVal) ? [...varsVal] : [];

    var optsVal = ui.varOptions.value();
    var currentList = Array.isArray(optsVal) ? [...optsVal] : [];

    var newList = [];

    for (let i = 0; i < varsList.length; i++) {
        let varName = varsList[i];
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].varName === varName) {
                found = currentList[j];
                break;
            }
        }
        if (found === null) {
            newList.push({
                varName: varName,
                coding: "dummy",
                refLevel: "",
                standardize: false
            });
        } else {
            newList.push(found);
        }
    }
    ui.varOptions.setValue(newList);
}

module.exports = events;
