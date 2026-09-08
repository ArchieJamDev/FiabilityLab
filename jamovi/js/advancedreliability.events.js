
// EN: The "factors" ListBox's item label (bound to the Array/Group's
// `label` field via valueKey) starts out NULL for a row created by
// clicking "Add New Factor" -- unlike the first row, which comes
// pre-populated from the .a.yaml option's own `default:` value. A row
// whose label has never been given a value appears to leave that row's
// own identity unresolved in jamovi's client-side data model, which in
// turn breaks the sibling VariablesListBox's ability to accumulate more
// than one dragged item for that row (confirmed empirically: naming a
// new row before dragging items into it works correctly every time;
// dragging items into an unnamed row does not). Auto-filling "Factor N"
// the instant a row is created -- the same fix jamovi's own built-in
// Confirmatory Factor Analysis module uses for its own "Add New Factor"
// list, reverse-engineered from its compiled UI bundle since this
// analysis ships with jamovi and has no visible .u.yaml/events source --
// removes the need for the user to know or follow that ordering.
// ES: La etiqueta del ítem de la ListBox "factors" (vinculada al campo
// `label` del Array/Group vía valueKey) empieza en NULL para una fila
// creada al hacer clic en "Add New Factor" -- a diferencia de la primera
// fila, que viene prepoblada desde el propio valor `default:` de la
// opción en .a.yaml. Una fila cuya etiqueta nunca recibió un valor
// parece dejar la identidad de esa fila sin resolver en el modelo de
// datos del lado del cliente de jamovi, lo que a su vez rompe la
// capacidad de la VariablesListBox hermana de acumular más de un ítem
// arrastrado para esa fila (confirmado empíricamente: nombrar una fila
// nueva antes de arrastrar ítems funciona correctamente siempre;
// arrastrar ítems a una fila sin nombre no). Rellenar automáticamente
// "Factor N" en el instante en que se crea una fila -- el mismo arreglo
// que usa el propio módulo nativo de Análisis Factorial Confirmatorio de
// jamovi para su propia lista "Add New Factor", obtenido por ingeniería
// inversa de su paquete de UI compilado ya que este análisis viene con
// jamovi y no tiene fuente .u.yaml/events visible -- elimina la
// necesidad de que el usuario conozca o siga ese orden.

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

    onEvent_listItemAdded: function(ui, data) {
        updateFactorLabels(ui);
        setTimeout(() => {
            data.item.controls[0].$input.focus();
        }, 0);
    },

    onEvent_listItemRemoved: function(ui) {
        updateFactorLabels(ui);
    }
};

module.exports = events;
