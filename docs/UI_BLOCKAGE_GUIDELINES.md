# Handling UI Blockage in jamovi Plugins

This document outlines the causes of the "Communication Bridge Blockage" (UI freeze) encountered during the development of the SuperCode plugin and defines the Best Practices for building responsive, reliable jamovi interfaces.

## 1. The Anatomy of a Bridge Freeze

A "Bridge Freeze" occurs when the data binding channel between the **Frontend (JavaScript/HTML)** and the **Backend (R Engine)** becomes saturated or enters an infinite loop. In jamovi, options are synchronized bi-directionally. If not managed carefully, this can lead to a "Recursive Event Storm."

### Common Causes:
1.  **The `applyToItems` Trap**: Calling `ui.listbox.applyToItems()` or similar UI-refresh methods inside an `onChange` event of the same control. This forces a DOM re-render while jamovi is still processing the event, often discarding uncommitted state and triggering secondary events.
2.  **Unchecked `setValue` Cycles**: Calling `ui.option.setValue()` inside an `onChange` handler for that same option without a "changed" check or a synchronization lock.
3.  **Recursive DOM Observers**: Using `MutationObserver` to watch DOM changes and calling `setPropertyValue` or `setValue` in response. Since these methods trigger renders, they can cause the observer to fire again immediately, locking the browser thread.
4.  **Backend Recursion**: Calling methods like `self$results$asProtoBuf()` inside the R `.run()` function. Since `.run` is part of the serialization process, this can trigger recursive serialization and crash the R process.

## 2. Best Practices

### A. The "Data-First" Rule (Physical vs. Logical Disabling)
*   **Logical (Data)**: Use jamovi's native `enable: (condition)` in `supercode.a.yaml`. This ensures backend consistency.
*   **Physical (UI)**: To visually disable elements, add a CSS class to the parent container (e.g., `sc-visual-disabled`) using `pointer-events: none`.
*   **Avoid**: Do NOT modify the `input.disabled` attribute directly. Jamovi manages these DOM nodes; external interference can break event listeners and cause unclickable components.

### B. Implement the "Safe-Sync" Helper
Always use a wrapper for `setValue` that implements:
1.  **Deep Equality Check**: Use `JSON.stringify` to compare the new value with the current one. Only proceed if they differ.
2.  **Option-Specific Locks**: Use flags like `this._syncing_varOptions` to prevent a `setValue` from being processed while another is in flight.
3.  **Asynchronous Decoupling**: Wrap `setValue` in a `setTimeout(..., 0)`. This allows jamovi to finish its current event cycle before processing the new value.

### C. Lightweight R Introspection
*   **Avoid `asProtoBuf()` in `.run()`**: It is heavy and dangerous for synchronization.
*   **Use Memory Access**: To check current results state (like existing column titles), access private members like `self$results$outputCols$.__enclos_env__$private$.titles` directly. This is synchronous, fast, and does not involve the communication bridge.

### D. YAML Over JavaScript
*   Favor YAML properties (`variable: var`, `items: (vars)`) over manual JavaScript synchronization.
*   The more jamovi handles natively, the less chance there is for a "Bridge Storm."

## 3. Implementation Example

```javascript
function safeSetValue(ui, optionName, value, context) {
    const lock = `_syncing_${optionName}`;
    if (context[lock]) return;

    if (JSON.stringify(ui[optionName].value()) === JSON.stringify(value))
        return;

    context[lock] = true;
    setTimeout(() => {
        try {
            ui[optionName].setValue(value);
        } finally {
            context[lock] = false;
        }
    }, 0);
}
```
