const REF_LEVEL_CODINGS = new Set(["dummy", "simple", "deviation"]);
const INTEGER_ONLY_CODINGS = new Set(["dummy", "deviation", "poly"]);

const events = {
    update: function(ui) {
        console.log("SuperCode: [update] starting...");
        ui._activeInstance = this;
        
        // 1. Stable ID initialization
        if (!ui.analysisId.value()) {
            const randomId = Math.random().toString(36).substring(2, 10);
            console.log("SuperCode: [update] generating new analysisId:", randomId);
            ui.analysisId.setValue(randomId);
        }

        // 2. Initial sync
        synchronizeVarOptions(ui, this);
        
        // 3. Setup styles and initial UI
        ensureDisabledStyles();
        refreshUIWithLogs(ui);
        updateOutputButton(ui);
    },

    onChange_vars: function(ui) {
        console.log("SuperCode: [onChange_vars] triggered");
        if (ui._activeInstance !== this) return;
        synchronizeVarOptions(ui, this);
        refreshUIWithLogs(ui);
    },

    onChange_varOptions: function(ui) {
        console.log("SuperCode: [onChange_varOptions] triggered");
        if (ui._activeInstance !== this) return;
        if (this._syncing) {
            console.log("SuperCode: [onChange_varOptions] skipping due to _syncing lock");
            return;
        }

        handleLogicConstraints(ui, this);
        refreshUIWithLogs(ui);
        updateOutputButton(ui);
    },

    onChange_codePrefix: function(ui) {
    },

    onChange_outputCols: function(ui) {
        if (ui._activeInstance !== this) return;
        updateOutputButton(ui);
    }
};

function supportsReferenceLevel(coding) {
    return REF_LEVEL_CODINGS.has(coding);
}

function supportsIntegerize(coding) {
    return !INTEGER_ONLY_CODINGS.has(coding);
}

function synchronizeVarOptions(ui, context) {
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

    if (newList.length !== currentList.length) changed = true;

    if (changed) {
        console.log("SuperCode: [synchronizeVarOptions] setting new value for varOptions");
        context._syncing = true;
        ui.varOptions.setValue(newList);
        context._lastVarOptions = newList.map(item => ({ ...item }));
        context._syncing = false;
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
}

function handleLogicConstraints(ui, context) {
    const currentList = ui.varOptions.value() || [];
    const lastList = context._lastVarOptions || [];

    let hasLogicalChange = false;
    const newList = currentList.map((item, idx) => {
        if (!item) return item;
        const lastItem = lastList[idx] || {};

        let coding = item.coding || "dummy";
        let ref = item.ref;
        let standardize = !!item.standardize;
        let integerize = !!item.integerize;

        // Constraint 1: Coding rules
        if (!supportsReferenceLevel(coding)) {
            if (ref !== null) {
                console.log(`SuperCode: [logic] clearing ref for ${item.var} due to coding: ${coding}`);
                ref = null;
                hasLogicalChange = true;
            }
        }
        if (!supportsIntegerize(coding)) {
            if (integerize !== false) {
                console.log(`SuperCode: [logic] clearing integerize for ${item.var} due to coding: ${coding}`);
                integerize = false;
                hasLogicalChange = true;
            }
        }

        // Constraint 2: Mutual Exclusivity (Std vs Int)
        // Only swap values if BOTH are true, based on what changed last.
        if (standardize && integerize) {
            const wasStd = !!lastItem.standardize;
            const wasInt = !!lastItem.integerize;

            if (standardize && !wasStd) {
                console.log(`SuperCode: [logic] Std ticked for ${item.var}, disabling Int`);
                integerize = false;
                hasLogicalChange = true;
            } else if (integerize && !wasInt) {
                console.log(`SuperCode: [logic] Int ticked for ${item.var}, disabling Std`);
                standardize = false;
                hasLogicalChange = true;
            }
        }

        return { ...item, coding, ref, standardize, integerize };
    });

    if (hasLogicalChange) {
        console.log("SuperCode: [handleLogicConstraints] applying logical fixes via setValue");
        context._syncing = true;
        ui.varOptions.setValue(newList);
        context._lastVarOptions = newList.map(item => ({ ...item }));
        context._syncing = false;
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
}

function ensureDisabledStyles() {
    if (document.getElementById('supercode-disabled-overlay-style')) return;
    const style = document.createElement('style');
    style.id = 'supercode-disabled-overlay-style';
    style.textContent = `
        .sc-visual-disabled {
            opacity: 0.35 !important;
            pointer-events: none !important;
            filter: grayscale(1);
        }
    `;
    document.head.appendChild(style);
}

/**
 * Diagnostic refresh of UI states.
 */
function refreshUIWithLogs(ui) {
    if (!ui.varOptions || !ui.varOptions.applyToItems) {
        console.warn("SuperCode: [refreshUI] varOptions or applyToItems missing");
        return;
    }
    const dlist = ui.varOptions.value() || [];

    ui.varOptions.applyToItems(0, (item, index, column) => {
        if (!item) return;
        const row = dlist[index] || {};
        const el = item.el || (item.$el && item.$el[0]);

        if (column === 2) { // Ref
            const disabled = !supportsReferenceLevel(row.coding);
            if (el) {
                const wasDisabled = el.classList.contains('sc-visual-disabled');
                if (wasDisabled !== disabled) {
                    console.log(`SuperCode: [refreshUI] Ref column for row ${index} -> disabled: ${disabled}`);
                    el.classList.toggle('sc-visual-disabled', disabled);
                }
            }
            // Populate levels
            if (item.variable !== row.var) {
                if (typeof item.setPropertyValue === 'function') {
                    item.setPropertyValue('variable', row.var);
                }
            }
        }
        else if (column === 4) { // Int
            // ONLY disable based on coding support. 
            // DO NOT disable just because Std is checked.
            const disabled = !supportsIntegerize(row.coding);
            if (el) {
                const wasDisabled = el.classList.contains('sc-visual-disabled');
                if (wasDisabled !== disabled) {
                    console.log(`SuperCode: [refreshUI] Int column for row ${index} -> disabled: ${disabled}`);
                    el.classList.toggle('sc-visual-disabled', disabled);
                }
            }
        }
    });
}

function ensureOutputButtonStyles() {
    if (document.getElementById('supercode-output-button-style')) return;
    let style = document.createElement('style');
    style.id = 'supercode-output-button-style';
    style.textContent = `
        .jmv-action-button.supercode-output-button.supercode-output-remove {
            background: #ffffff; border: 1px solid #c2410c; color: #c2410c;
        }
        .jmv-action-button.supercode-output-button.supercode-output-remove:hover { background: #fff7ed; }
        .jmv-action-button.supercode-output-button.supercode-output-disabled {
            color: #c5c5c5; background-color: #ababab; border: 1px solid #ababab; cursor: default;
        }
    `;
    document.head.appendChild(style);
}

function updateOutputButton(ui) {
    if (!ui.outputCols) return;
    let root = ui.outputCols.el || ui.outputCols._subel;
    if (!root) return;

    let input = ui.outputCols.input || root.querySelector('input[type="checkbox"]');
    let text = ui.outputCols.label || root.querySelector('span');
    let label = text ? text.parentElement : root.querySelector('label');

    if (!input || !text || !label) return;

    ensureOutputButtonStyles();
    if (input.dataset.supercodeButtonBound !== 'true') {
        input.dataset.supercodeButtonBound = 'true';
        input.addEventListener('change', () => updateOutputButton(ui));
    }

    input.style.cssText = 'position:absolute;opacity:0;width:1px;height:1px;pointer-events:none;';
    label.classList.add('jmv-action-button', 'supercode-output-button');
    label.style.cursor = input.disabled ? 'default' : 'pointer';

    text.textContent = input.checked ? 'Remove Columns' : 'Add Columns';
    label.classList.toggle('supercode-output-remove', input.checked && !input.disabled);
    label.classList.toggle('supercode-output-disabled', input.disabled);
}

module.exports = events;
