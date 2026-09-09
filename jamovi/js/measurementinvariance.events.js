// Same fix as advancedreliability.events.js: a row added via "Add New
// Factor" gets label: NULL until something sets it, which breaks its
// VariablesListBox's ability to accumulate more than one dragged item.
// Auto-filling "Factor N" on row creation avoids that.

const updateFactorLabels = function(ui) {
    ui.factors.applyToItems(0, (item, index) => {
        let value = item.controls[0].value();
        if (!value || value.trim() === '')
            item.controls[0].setValue('Factor ' + (index + 1));
    });
};

const events = {

    update: function(ui) {
        updateFactorLabels(ui);
    },

    onEvent_listItemAdded: function(ui) {
        updateFactorLabels(ui);
    },

    onEvent_listItemRemoved: function(ui) {
        updateFactorLabels(ui);
    }
};

module.exports = events;
