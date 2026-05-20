const REF_LEVEL_CODINGS = new Set(["dummy", "simple", "deviation"]);

const events = {
    update: function(ui) {
        updateVarOptions(ui);
        updateOutputButton(ui);
    },

    view_updated: function(ui) {
        updateOutputButton(ui);
    },

    onChange_vars: function(ui) {
        updateVarOptions(ui);
        updateOutputButton(ui);
    },

    onChange_varOptions: function(ui) {
        updateVarOptions(ui);
        updateOutputButton(ui);
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

function ensureOutputButtonStyles() {
    if (document.getElementById('supercode-output-button-style'))
        return;

    let style = document.createElement('style');
    style.id = 'supercode-output-button-style';
    style.textContent = `
        .jmv-action-button.supercode-output-button.supercode-output-remove {
            background: #ffffff;
            background-image: none;
            border: 1px solid #c2410c;
            box-shadow: none;
            color: #c2410c;
        }

        .jmv-action-button.supercode-output-button.supercode-output-remove:hover {
            background: #fff7ed;
            background-image: none;
        }

        .jmv-action-button.supercode-output-button.supercode-output-remove:active:hover {
            background: #ffedd5;
        }

        .jmv-action-button.supercode-output-button.supercode-output-disabled {
            color: #c5c5c5;
            background-color: #ababab;
            background-image: none;
            box-shadow: none;
            border: 1px solid #ababab;
            cursor: default;
        }
    `;
    document.head.appendChild(style);
}

function updateOutputButton(ui) {
    let control = ui.outputCols;
    if (!control)
        return;

    let root = control.el || control._subel;
    if (!root)
        return;

    let input = control.input || root.querySelector('input[type="checkbox"]');
    let text = control.label || root.querySelector('span');
    let label = text ? text.parentElement : root.querySelector('label');

    if (!input || !text || !label)
        return;

    ensureOutputButtonStyles();
    bindOutputButtonEvents(ui, input);

    input.style.position = 'absolute';
    input.style.opacity = '0';
    input.style.width = '1px';
    input.style.height = '1px';
    input.style.margin = '0';
    input.style.pointerEvents = 'none';

    label.classList.add('jmv-action-button', 'supercode-output-button');
    label.style.cursor = input.disabled ? 'default' : 'pointer';

    applyOutputButtonState(label, text, input.checked, input.disabled);
}

function bindOutputButtonEvents(ui, input) {
    if (input.dataset.supercodeButtonBound === 'true')
        return;

    input.dataset.supercodeButtonBound = 'true';
    input.addEventListener('change', () => updateOutputButton(ui));
}

function applyOutputButtonState(label, text, checked, disabled) {
    text.textContent = checked ? 'Remove Columns' : 'Add Columns';
    label.classList.toggle('supercode-output-remove', checked && !disabled);
    label.classList.toggle('supercode-output-disabled', disabled);
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
