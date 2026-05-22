const REF_LEVEL_CODINGS = new Set(["dummy", "simple", "deviation"]);
const INTEGER_ONLY_CODINGS = new Set(["dummy", "deviation", "poly"]);

const events = {
    update: function(ui) {
        try {
            ui._activeInstance = this;
            
            // 1. Immediate try (might catch some cases)
            synchronizeVarOptions(ui, this);
            updateLevelControls(ui);
            updateOutputButton(ui);

            // 2. Reactive catch-up: Use MutationObserver to detect when the table actually renders
            // This is far more robust than a fixed timeout.
            if (ui.varOptions && ui.varOptions.el) {
                if (this._observer) this._observer.disconnect();
                
                this._observer = new MutationObserver(() => {
                    if (ui._activeInstance !== this) return;
                    updateLevelControls(ui);
                });
                
                this._observer.observe(ui.varOptions.el, { 
                    childList: true, 
                    subtree: true 
                });
            }

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
            
            // If we are currently in a deferred sync, we still want to record the last state,
            // but we don't want to trigger a new cycle of mutual exclusivity checks yet.
            if (this._syncing) return;
            
            if (!this._lastVarOptions) {
                this._lastVarOptions = (ui.varOptions.value() || []).map(item => ({ ...item }));
            }
            runOnChangeVarOptions(ui, this);
            updateLevelControls(ui);
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

function supportsReferenceLevel(coding) {
    return REF_LEVEL_CODINGS.has(coding);
}

function supportsIntegerize(coding) {
    return !INTEGER_ONLY_CODINGS.has(coding);
}

/**
 * Uses a deferred update to avoid blocking the communication bridge.
 * This "delays" the setValue to the next tick, allowing jamovi to finish current cycle.
 */
function synchronizeVarOptions(ui, context) {
    if (!ui || !ui.vars || !ui.varOptions || context._syncing) return;

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
        context._syncing = true;
        // DEFER: Push to next tick to let the bridge breathe
        setTimeout(() => {
            try {
                ui.varOptions.setValue(newList);
                context._lastVarOptions = newList.map(item => ({ ...item }));
                updateLevelControls(ui);
            } finally {
                context._syncing = false;
            }
        }, 0);
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
}

function runOnChangeVarOptions(ui, context) {
    const currentList = ui.varOptions.value() || [];
    if (!context._lastVarOptions) {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
        return;
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

        if (!supportsReferenceLevel(coding)) ref = null;
        if (!supportsIntegerize(coding)) integerize = false;

        // Mutual exclusivity check
        if (standardize && integerize) {
            const lastStd = !!lastItem.standardize;
            const lastInt = !!lastItem.integerize;

            if (standardize !== lastStd) {
                integerize = false;
                changed = true;
            } else if (integerize !== lastInt) {
                standardize = false;
                changed = true;
            } else {
                integerize = false;
                changed = true;
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
        context._syncing = true;
        // DEFER: Push to next tick
        setTimeout(() => {
            try {
                ui.varOptions.setValue(newList);
                context._lastVarOptions = newList.map(item => ({ ...item }));
                // After a deferred setValue, we need to refresh the HTML states
                updateLevelControls(ui);
            } finally {
                context._syncing = false;
            }
        }, 0);
    } else {
        context._lastVarOptions = currentList.map(item => ({ ...item }));
    }
}

function updateLevelControls(ui) {
    if (!ui || !ui.varOptions) return;
    const dlist = ui.varOptions.value();
    if (!Array.isArray(dlist)) return;

    if (typeof ui.varOptions.applyToItems !== 'function') return;

    ui.varOptions.applyToItems(0, (item, index, column) => {
        if (!item) return;
        const row = dlist[index] || {};

        if (column === 2) { // Reference Level
            const enabled = supportsReferenceLevel(row.coding);
            item.setPropertyValue('variable', row.var);
            item.setPropertyValue('enable', enabled);
            if (item.input) item.input.disabled = !enabled;
        }
        else if (column === 4) { // Integerize
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
