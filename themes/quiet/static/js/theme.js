/* Theme toggle + the accent picker hidden behind five clicks on the header.
   All the colour maths lives in window.SiteTheme (set inline in <head>),
   so there is only one implementation of it. */
(function () {
  var T = window.SiteTheme;
  if (!T) return;

  var header = document.querySelector('.topbar');
  var toggle = document.querySelector('.theme-toggle');

  function track(name, data) {
    if (window.siteTrack) window.siteTrack(name, data);
  }

  /* ---------- Theme toggle ----------

     Light and dark. With no stored preference the page follows the
     system and keeps following it if the system changes later. The icon
     shows the colours on screen (sun for light, moon for dark). */

  var darkQuery = window.matchMedia
    ? window.matchMedia('(prefers-color-scheme: dark)')
    : null;

  function systemTheme() {
    return darkQuery && darkQuery.matches ? 'dark' : 'light';
  }

  /* Two states. Every press flips the colours on screen, so the first
     press always does something visible. Until the reader presses, the
     page follows the system. */
  function nextTheme() {
    return T.isDark() ? 'light' : 'dark';
  }

  function syncToggle() {
    if (!toggle) return;
    var pref = T.isDark() ? 'dark' : 'light';
    var next = nextTheme();

    /* The icon is chosen in CSS from <html data-theme-pref>, set before
       the first paint. This mirror is here for anything scripting the
       button itself. */
    toggle.dataset.pref = pref;

    /* Not aria-pressed: that describes two states, and there are three. */
    toggle.removeAttribute('aria-pressed');

    var label = 'Theme: ' + pref + '. Switch to ' + next + '.';

    toggle.setAttribute('aria-label', label);
    /* Also on hover - three states behind one icon is a lot to infer. */
    toggle.setAttribute('title', label);
  }

  if (toggle) {
    toggle.addEventListener('click', function () {
      var next = nextTheme();
      T.write('theme', next);          // only written once they interact
      T.setTheme(next);
      syncToggle();
      track('theme-toggle', { to: next });
    });
    syncToggle();
  }

  /* Follow the system while they haven't chosen for themselves. The button
     is resynced either way: its label names the system's colour, so it can
     go stale even when the page itself doesn't move. */
  if (darkQuery) {
    var onSystemChange = function () {
      if (!T.savedTheme()) T.setTheme(null);   // still following: re-apply
      syncToggle();
    };
    if (darkQuery.addEventListener) darkQuery.addEventListener('change', onSystemChange);
    else if (darkQuery.addListener) darkQuery.addListener(onSystemChange);
  }

  /* ---------- Accent picker ---------- */

  var panel  = document.querySelector('.accent-panel');
  if (!panel || !header) return;

  var select = panel.querySelector('.accent-select');
  var custom = panel.querySelector('.accent-custom');
  var reset  = panel.querySelector('.accent-reset');
  var close  = panel.querySelector('.accent-close');

  function openPanel() {
    panel.hidden = false;
    var current = T.accent();
    if (custom) custom.value = current;
    if (select) {
      // Match the dropdown to the current colour if it's one of the presets.
      select.value = current.toLowerCase();
      if (select.value !== current.toLowerCase()) select.selectedIndex = -1;
      select.focus();
    }
  }

  function closePanel() {
    panel.hidden = true;
  }

  function choose(hex) {
    if (!/^#[0-9a-f]{6}$/i.test(hex)) return;
    T.setAccent(hex);
    if (custom) custom.value = hex;
    syncToggle();
  }

  /* Dragging the colour input fires `input` on every pixel of the gradient.
     Record where they stopped, not the hundred colours they passed over. */
  var accentTimer = null;
  function trackAccent(hex, via) {
    clearTimeout(accentTimer);
    accentTimer = setTimeout(function () {
      track('accent-change', { colour: String(hex).toLowerCase(), via: via });
    }, 700);
  }

  if (select) {
    select.addEventListener('change', function () {
      choose(select.value);
      trackAccent(select.value, 'preset');
    });
  }
  if (custom) {
    custom.addEventListener('input', function () {
      choose(custom.value);
      trackAccent(custom.value, 'custom');
    });
  }

  if (reset) {
    reset.addEventListener('click', function () {
      T.write('theme', null);
      T.write('accent', null);
      T.setTheme(null);                // back to following the system
      syncToggle();
      if (custom) custom.value = T.DEFAULT_ACCENT;
      if (select) select.value = T.DEFAULT_ACCENT;
      track('accent-reset');
    });
  }

  if (close) close.addEventListener('click', closePanel);

  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape' && !panel.hidden) closePanel();
  });

  /* Five clicks on the header - but not on anything you could have
     meant to click, or navigating the site would trip it constantly. */
  var clicks = 0, timer = null;

  header.addEventListener('click', function (e) {
    if (e.target.closest('a, button, input, select, label')) return;

    clicks++;
    clearTimeout(timer);
    timer = setTimeout(function () { clicks = 0; }, 1500);

    if (clicks >= 5) {
      clicks = 0;
      openPanel();
      track('easter-egg-accent-panel', { from: location.pathname });
    }
  });
})();
