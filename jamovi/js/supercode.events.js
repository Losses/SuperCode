const REF_LEVEL_CODINGS = new Set(["dummy", "simple", "deviation"]);
const INTEGER_ONLY_CODINGS = new Set(["dummy", "deviation", "poly"]);

const events = {
    update: function(ui) {
        try {
            ui._activeInstance = this;
            
            // 1. Initialize ID if missing
            if (!ui.analysisId.value()) {
                safeSetValue(ui, 'analysisId', Math.random().toString(36).substring(2, 10), this);
            }

            // 2. Initial sync for table population
            synchronizeVarOptions(ui, this);

            // 3. PHYSICAL DISABLE OBSERVER
            // This is DOM-ONLY. It reacts to jamovi's rendering.
            if (ui.varOptions && ui.varOptions.el) {
                if (this._observer) this._observer.disconnect();
                this._observer = new MutationObserver(() => {
                    if (ui._activeInstance !== this) return;
                    // TRULY READ-ONLY: Only touches HTML attributes
                    applyPhysicalDisableOnly(ui);
                });
                this._observer.observe(ui.varOptions.el, { childList: true, subtree: true });
            }

            applyPhysicalDisableOnly(ui);
            updateOutputButton(ui);

            this._initialized = true;
        } catch (e) {
            console.error("Error in update:", e);
        }
    },

    onChange_vars: function(ui) {
        try {
            if (ui._activeInstance !== this) return;
            synchronizeVarOptions(ui, this);
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in onChange_vars:", e);
        }
    },

    onChange_varOptions: function(ui) {
        try {
            if (ui._activeInstance !== this) return;
            if (this._syncing) return;
            
            if (!this._lastVarOptions) {
                this._lastVarOptions = (ui.varOptions.value() || []).map(item => ({ ...item }));
            }
            
            runOnChangeVarOptions(ui, this);
            applyPhysicalDisableOnly(ui);
            updateOutputButton(ui);
        } catch (e) {
            console.error("Error in onChange_varOptions:", e);
        }
    },

    onChange_codePrefix: function(ui) {
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

/**
 * Robust setValue helper with deep comparison and recursion guard.
 */
function safeSetValue(ui, optionName, value, context) {
    if (!ui[optionName] || context._syncing) return;
    
    const current = ui[optionName].value();
    if (JSON.stringify(current) === JSON.stringify(value)) return;

    context._syncing = true;
    // Defer to next tick to avoid blocking the current event chain
    setTimeout(() => {
        try {
            ui[optionName].setValue(value);
            if (optionName === 'varOptions') {
                context._lastVarOptions = (value || []).map(item => ({ ...item }));
                applyPhysicalDisableOnly(ui);
            }
        } finally {
            context._syncing = false;
        }
    }, 0);
}

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

    if (newList.length !== currentList.length) {
        changed = true;
    }

    if (changed) {
        safeSetValue(ui, 'varOptions', newList, context);
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
}

function runOnChangeVarOptions(ui, context) {
    const currentList = ui.varOptions.value() || [];
    const lastList = context._lastVarOptions || [];
    
    let changed = false;
    const newList = currentList.map((item, idx) => {
        if (!item) return item;
        const lastItem = lastList[idx] || {};

        let coding = item.coding || "dummy";
        let standardize = !!item.standardize;
        let integerize = !!item.integerize;

        // Mutual exclusivity check
        if (standardize && integerize) {
            const lastStd = !!lastItem.standardize;
            const lastInt = !!lastItem.integerize;
            if (standardize !== lastStd) integerize = false;
            else if (integerize !== lastInt) standardize = false;
            else integerize = false;
            changed = true;
        }

        return { ...item, standardize, integerize };
    });

    if (changed) {
        safeSetValue(ui, 'varOptions', newList, context);
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
}

/**
 * CRITICAL: This function must be TRULY READ-ONLY for the jamovi data model.
 * It ONLY modifies the HTML 'disabled' attribute.
 * It NEVER calls setPropertyValue.
 */
function applyPhysicalDisableOnly(ui) {
    if (!ui || !ui.varOptions || !ui.varOptions.applyToItems) return;
    const dlist = ui.varOptions.value();
    if (!Array.isArray(dlist)) return;

    ui.varOptions.applyToItems(0, (item, index, column) => {
        if (!item || !item.input) return;
        const row = dlist[index] || {};

        if (column === 2) { // Reference Level
            const enabled = supportsReferenceLevel(row.coding);
            // No setPropertyValue here! YAML handles the model.
            if (item.input.disabled !== !enabled) {
                item.input.disabled = !enabled;
            }
        }
        else if (column === 4) { // Integerize
            const enabled = supportsIntegerize(row.coding);
            if (item.input.disabled !== !enabled) {
                item.input.disabled = !enabled;
            }
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
            border: 1px solid #c2410c;
            color: #c2410c;
        }
        .jmv-action-button.supercode-output-button.supercode-output-remove:hover {
            background: #fff7ed;
        }
        .jmv-action-button.supercode-output-button.supercode-output-disabled {
            color: #c5c5c5;
            background-color: #ababab;
            border: 1px solid #ababab;
            cursor: default;
        }
    `;
    document.head.appendChild(style);
}

function updateOutputButton(ui) {
    if (!ui || !ui.outputCols) return;
    let control = ui.outputCols;
    let root = control.el || control._subel;
    if (!root) return;

    let input = control.input || (typeof root.querySelector === 'function' ? root.querySelector('input[type="checkbox"]') : null);
    let text = control.label || (typeof root.querySelector === 'function' ? root.querySelector('span') : null);
    let label = text ? text.parentElement : (typeof root.querySelector === 'function' ? root.querySelector('label') : null);

    if (!input || !text || !label) return;

    ensureOutputButtonStyles();
    if (input.dataset.supercodeButtonBound !== 'true') {
        input.dataset.supercodeButtonBound = 'true';
        input.addEventListener('change', () => updateOutputButton(ui));
    }

    input.style.position = 'absolute';
    input.style.opacity = '0';
    input.style.width = '1px';
    input.style.height = '1px';
    input.style.pointerEvents = 'none';

    label.classList.add('jmv-action-button', 'supercode-output-button');
    label.style.cursor = input.disabled ? 'default' : 'pointer';

    text.textContent = input.checked ? 'Remove Columns' : 'Add Columns';
    label.classList.toggle('supercode-output-remove', input.checked && !input.disabled);
    label.classList.toggle('supercode-output-disabled', input.disabled);
}

module.exports = events;
