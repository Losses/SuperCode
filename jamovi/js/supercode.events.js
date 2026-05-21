const REF_LEVEL_CODINGS = new Set(["dummy", "simple", "deviation"]);
const INTEGER_ONLY_CODINGS = new Set(["dummy", "deviation", "poly"]); // 整数 == 干净，Int 无意义

const events = {
    update: function(ui) {
        try {
            if (ui && ui._loaded) {
                updateVarOptions(ui);
            }
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in update:", e);
        }
    },

    view_updated: function(ui) {
        try {
            if (ui) {
                ui._loaded = true;
                updateVarOptions(ui);
            }
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in view_updated:", e);
        }
    },

    onChange_vars: function(ui) {
        try {
            if (ui) {
                ui._loaded = true;
                updateVarOptions(ui);
            }
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in onChange_vars:", e);
        }
    },

    onChange_varOptions: function(ui) {
        try {
            if (ui) {
                ui._loaded = true;
                updateVarOptions(ui);
            }
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in onChange_varOptions:", e);
        }
    }
};

function supportsReferenceLevel(coding) {
    return REF_LEVEL_CODINGS.has(coding);
}

function supportsIntegerize(coding)     {
    return !INTEGER_ONLY_CODINGS.has(coding);
}

function findByVar(list, varName) {
    if (!Array.isArray(list)) return null;
    for (const item of list) if (item.var === varName) return item;
    return null;
}

function normalizeVarOption(option, varName, prev) {
    let coding      = option && option.coding ? option.coding : "dummy";
    let ref         = supportsReferenceLevel(coding) && option && option.ref ? option.ref : "";
    let standardize = !!(option && option.standardize === true);
    let integerize  = !!(option && option.integerize  === true);

    if (!supportsIntegerize(coding))
        integerize = false;

    if (standardize && integerize) {
        const prevStd = !!(prev && prev.standardize === true);
        const prevInt = !!(prev && prev.integerize  === true);
        if      (prevStd && !prevInt) standardize = false;
        else if (prevInt && !prevStd) integerize  = false;
        else                          integerize  = false;
    }

    return { var: varName, coding, ref, standardize, integerize };
}

function varOptionsAreEqual(list1, list2) {
    if (!Array.isArray(list1) || !Array.isArray(list2)) return false;
    if (list1.length !== list2.length) return false;
    for (let i = 0; i < list1.length; i++) {
        const o1 = list1[i];
        const o2 = list2[i];
        if (!o1 || !o2) return false;
        if (o1.var !== o2.var ||
            o1.coding !== o2.coding ||
            o1.ref !== o2.ref ||
            o1.standardize !== o2.standardize ||
            o1.integerize !== o2.integerize) {
            return false;
        }
    }
    return true;
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
    if (!ui || !ui.outputCols)
        return;
    let control = ui.outputCols;

    let root = control.el || control._subel;
    if (!root)
        return;

    let input = control.input;
    if (!input && typeof root.querySelector === 'function') {
        input = root.querySelector('input[type="checkbox"]');
    }
    let text = control.label;
    if (!text && typeof root.querySelector === 'function') {
        text = root.querySelector('span');
    }
    let label = text ? text.parentElement : null;
    if (!label && typeof root.querySelector === 'function') {
        label = root.querySelector('label');
    }

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
    if (!ui || !ui.vars || !ui.varOptions) return;
    const varsList    = Array.isArray(ui.vars.value())       ? [...ui.vars.value()]       : [];
    const currentList = Array.isArray(ui.varOptions.value()) ? [...ui.varOptions.value()] : [];

    const prevSnapshot = ui._lastVarOptionsSnapshot || null;

    const newList = varsList.map(varName => {
        const found = findByVar(currentList, varName);
        const prev  = findByVar(prevSnapshot, varName);
        return normalizeVarOption(found, varName, prev);
    });

    if (!varOptionsAreEqual(currentList, newList))
        ui.varOptions.setValue(newList);

    ui._lastVarOptionsSnapshot = newList.map(item => ({ ...item }));

    updateLevelControls(ui);
}

function updateLevelControls(ui) {
    if (!ui || !ui.varOptions) return;
    const dlist = ui.varOptions.value();
    if (!Array.isArray(dlist)) return;

    if (typeof ui.varOptions.applyToItems !== 'function') return;

    ui.varOptions.applyToItems(0, (item, index, column) => {
        if (!item) return;
        const row = dlist[index] || {};

        if (column === 2) {
            const enabled = supportsReferenceLevel(row.coding);
            item.setPropertyValue('variable', row.var);
            item.setPropertyValue('enable', enabled);
            if (item.input) item.input.disabled = !enabled;
        }
        else if (column === 4) {
            const enabled = supportsIntegerize(row.coding);
            item.setPropertyValue('enable', enabled);
            if (item.input) item.input.disabled = !enabled;
        }
    });
}

module.exports = events;
