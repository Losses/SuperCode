const REF_LEVEL_CODINGS = new Set(["dummy", "simple", "deviation"]);
const INTEGER_ONLY_CODINGS = new Set(["dummy", "deviation", "poly"]); // 整数 == 干净，Int 无意义

const events = {
    update: function(ui) {
        try {
            synchronizeVarOptions(ui, this);
            updateLevelControls(ui);
            updateOutputButton(ui);
            
            ui._activeInstance = this;
            this._initialized = true;
        } catch (e) {
            console.error("Error in update:", e);
        }
    },

    onChange_vars: function(ui) {
        try {
            if (ui._activeInstance !== this) return;
            synchronizeVarOptions(ui, this);
            updateLevelControls(ui);
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in onChange_vars:", e);
        }
    },

    onChange_varOptions: function(ui) {
        try {
            if (ui._activeInstance !== this) return;
            if (!this._lastVarOptions) {
                this._lastVarOptions = (ui.varOptions.value() || []).map(item => ({ ...item }));
            }
            runOnChangeVarOptions(ui, this);
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in onChange_varOptions:", e);
        }
    },

    onChange_outputCols: function(ui) {
        try {
            if (ui._activeInstance !== this) return;
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in onChange_outputCols:", e);
        }
    }
};

function supportsReferenceLevel(coding) {
    return REF_LEVEL_CODINGS.has(coding);
}

function supportsIntegerize(coding)     {
    return !INTEGER_ONLY_CODINGS.has(coding);
}

function synchronizeVarOptions(ui, context) {
    if (!ui || !ui.vars || !ui.varOptions) return;

    const vars = ui.vars.value() || [];
    const currentList = ui.varOptions.value() || [];

    let changed = false;
    const newList = [];

    for (let i = 0; i < vars.length; i++) {
        const v = vars[i];
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j] && currentList[j].var === v) {
                found = currentList[j];
                break;
            }
        }
        if (found === null) {
            newList.push({
                var: v,
                coding: "dummy",
                ref: null,
                standardize: false,
                integerize: false
            });
            changed = true;
        } else {
            newList.push(found);
        }
    }

    if (newList.length !== currentList.length) {
        changed = true;
    }

    if (changed) {
        ui.varOptions.setValue(newList);
        context._lastVarOptions = newList.map(item => ({ ...item }));
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
}

function runOnChangeVarOptions(ui, context) {
    const currentList = ui.varOptions.value() || [];
    if (!context._lastVarOptions) {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
    const lastList = context._lastVarOptions;

    let changed = false;
    const newList = currentList.map((item, idx) => {
        if (!item) return item;
        const lastItem = lastList[idx] || {};

        let coding = item.coding || "dummy";
        let ref = item.ref;
        let standardize = !!item.standardize;
        let integerize = !!item.integerize;

        // Clean ref if coding changed to something not supporting it
        if (!supportsReferenceLevel(coding)) {
            ref = null;
        }

        // Clean integerize if coding changed to something not supporting it
        if (!supportsIntegerize(coding)) {
            integerize = false;
        }

        // Mutual exclusivity check
        if (standardize && integerize) {
            const lastStd = !!lastItem.standardize;
            const lastInt = !!lastItem.integerize;

            if (standardize !== lastStd && integerize === lastInt) {
                // standardize became true, uncheck integerize
                integerize = false;
            } else if (integerize !== lastInt && standardize === lastStd) {
                // integerize became true, uncheck standardize
                standardize = false;
            } else {
                // fallback
                integerize = false;
            }
        }

        if (coding !== item.coding ||
            ref !== item.ref ||
            standardize !== item.standardize ||
            integerize !== item.integerize) {
            changed = true;
            return { var: item.var, coding, ref, standardize, integerize };
        }
        return item;
    });

    if (changed) {
        ui.varOptions.setValue(newList);
        context._lastVarOptions = newList.map(item => ({ ...item }));
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }

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

    let input = control.input || (typeof root.querySelector === 'function' ? root.querySelector('input[type="checkbox"]') : null);
    let text = control.label || (typeof root.querySelector === 'function' ? root.querySelector('span') : null);
    let label = text ? text.parentElement : (typeof root.querySelector === 'function' ? root.querySelector('label') : null);

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

module.exports = events;
