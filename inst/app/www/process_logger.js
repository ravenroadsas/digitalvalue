// Client-side buffering of raw user activity for process mining.
// Input changes and navigation are captured in the browser and sent to the
// server in batches (every FLUSH_MS) as a single input, so logging adds no
// per-keystroke round-trips to the Shiny R process.
(function () {
  var FLUSH_MS = 5000;
  var buffer = [];
  var last = {};
  var SKIP = /^\.clientdata|^dv_raw_events$|_(state|rows_current|rows_all|cell_clicked|cells_selected|columns_selected|cell_info|search|search_columns|clicked_data|clicked_serie|clicked_row|mouseover_data|mouseover_serie|mouseover_row|brush|legend_change|legend_selected)$/;

  function summarise(v) {
    if (v === null || v === undefined) return '';
    if (typeof v === 'object') v = JSON.stringify(v);
    v = String(v);
    return v.length > 120 ? v.substring(0, 117) + '...' : v;
  }

  $(document).on('shiny:inputchanged', function (e) {
    if (!e.name || SKIP.test(e.name)) return;
    // initial values of (re-)rendered widgets are not user activity
    if (e.inputType === 'shiny.action' && !e.value) return;
    if (/_rows_selected$/.test(e.name) && (e.value === null || e.value.length === 0)) return;
    var val = summarise(e.value);
    // text inputs fire on every keystroke: keep only the latest value per input
    var prev = buffer.length ? buffer[buffer.length - 1] : null;
    if (prev && prev.name === e.name && last[e.name] !== undefined &&
        typeof e.value === 'string' && e.name.indexOf('nav') !== 0) {
      prev.value = val; prev.ts = new Date().toISOString();
      return;
    }
    last[e.name] = val;
    buffer.push({ ts: new Date().toISOString(), name: e.name, value: val });
  });

  function flush() {
    if (!buffer.length || !window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue('dv_raw_events', JSON.stringify(buffer), { priority: 'event' });
    buffer = [];
  }
  setInterval(flush, FLUSH_MS);
  window.addEventListener('beforeunload', flush);
})();

// Charts and tables inside a collapsed card need a resize once it is shown
$(document).on('shown.bs.collapse', function () {
  window.dispatchEvent(new Event('resize'));
  $(window).trigger('resize');
});
