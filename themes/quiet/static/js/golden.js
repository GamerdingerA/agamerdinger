/* The homepage drawing.

   phi is 1.6180339887. A rectangle whose sides stand in that ratio can be
   cut into a square and a smaller rectangle of the same shape, over and
   over. Drawing the quarter circle inside each square gives the spiral.

   It draws itself once on load, drifts a little with the pointer, brightens
   as the pointer comes near, and turns a quarter when clicked. Colours come
   from the CSS custom properties, so the drawing follows the theme and the
   accent picker without knowing anything about either. window.drawGolden is
   called by theme.js whenever one of those changes. */
(function () {
  var PHI = (1 + Math.sqrt(5)) / 2;
  var DEPTH = 11;

  var still = window.matchMedia &&
    window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  var canvas, wrap, ctx;
  var W = 0, H = 0;
  var grow = still ? 1 : 0;      /* how much of the spiral is drawn, 0 to 1 */
  var turn = 0;                  /* quarter turns asked for */
  var turnNow = 0;               /* quarter turns currently drawn */
  var px = null, py = null;      /* pointer, in canvas coordinates */
  var driftX = 0, driftY = 0, wantX = 0, wantY = 0;
  var running = false;

  function css(name, fallback) {
    var v = getComputedStyle(document.documentElement)
      .getPropertyValue(name).trim();
    return v || fallback;
  }

  function isDark() {
    if (window.SiteTheme && window.SiteTheme.isDark) return window.SiteTheme.isDark();
    return !!(window.matchMedia &&
              window.matchMedia('(prefers-color-scheme: dark)').matches);
  }

  /* Each step turns the rectangle a quarter turn. The square sits on the
     side given by the step number, and what is left is the next
     rectangle. Returns the squares, largest first. */
  function squares(x, y, w, h, n) {
    var out = [];
    var side, dir = 0;
    for (var i = 0; i < n && w > 1 && h > 1; i++) {
      if (w >= h) {
        side = h;
        if (dir % 2 === 0) { out.push({ x: x, y: y, s: side }); x += side; }
        else { out.push({ x: x + w - side, y: y, s: side }); }
        w -= side;
      } else {
        side = w;
        if (dir % 2 === 1) { out.push({ x: x, y: y, s: side }); y += side; }
        else { out.push({ x: x, y: y + h - side, s: side }); }
        h -= side;
      }
      dir = (dir + 1) % 4;
    }
    return out;
  }

  /* The quarter circle inside one square, centred on whichever corner lies
     nearest the eye of the spiral, so neighbouring arcs meet end to end.
     `part` draws only the first part of it, which is what the opening
     animation uses. */
  function arc(sq, eye, part) {
    var corners = [
      [sq.x, sq.y], [sq.x + sq.s, sq.y],
      [sq.x, sq.y + sq.s], [sq.x + sq.s, sq.y + sq.s]
    ];
    var best = corners[0], bestD = Infinity;
    corners.forEach(function (c) {
      var d = (c[0] - eye[0]) * (c[0] - eye[0]) + (c[1] - eye[1]) * (c[1] - eye[1]);
      if (d < bestD) { bestD = d; best = c; }
    });

    var cx = best[0], cy = best[1];
    var mx = sq.x + sq.s / 2, my = sq.y + sq.s / 2;
    var a1 = mx > cx ? 0 : Math.PI;
    var a2 = my > cy ? Math.PI / 2 : -Math.PI / 2;
    var ccw = ((a2 - a1 + 2 * Math.PI) % (2 * Math.PI)) !== Math.PI / 2;
    var sweep = (Math.PI / 2) * Math.max(0, Math.min(1, part));
    var end = ccw ? a1 - sweep : a1 + sweep;

    ctx.beginPath();
    ctx.arc(cx, cy, sq.s, a1, end, ccw);
    ctx.stroke();
  }

  function draw() {
    if (!canvas) return;

    var ratio = Math.min(window.devicePixelRatio || 1, 2);
    W = wrap.clientWidth; H = wrap.clientHeight;
    if (canvas.width !== W * ratio || canvas.height !== H * ratio) {
      canvas.width = W * ratio;
      canvas.height = H * ratio;
      canvas.style.width = W + 'px';
      canvas.style.height = H + 'px';
    }
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    ctx.clearRect(0, 0, W, H);

    /* The figure sits in the right-hand part of the window, clear of the
       text, and always whole. */
    var maxW = W * (W < 700 ? 0.86 : 0.48);
    var maxH = H * 0.78;
    var h = Math.min(maxH, maxW / PHI);
    var w = h * PHI;
    var x = W < 700 ? (W - w) / 2 : W - w - Math.min(W * 0.04, 48);
    var y = (H - h) / 2;

    var sqs = squares(x, y, w, h, DEPTH);
    var last = sqs[sqs.length - 1];
    var eye = last ? [last.x + last.s / 2, last.y + last.s / 2] : [x, y];

    /* How close the pointer is to the figure, 0 to 1. Everything gets a
       little brighter as it comes near. */
    var near = 0;
    if (px !== null) {
      var d = Math.hypot(px - driftX - (x + w / 2), py - driftY - (y + h / 2));
      near = Math.max(0, 1 - d / (w * 0.9));
    }

    var dark = isDark();
    var ruleA = (dark ? 0.75 : 0.38) + near * 0.25;
    var arcLow = dark ? 0.32 : 0.12;
    var arcHigh = dark ? 0.95 : 0.46;

    ctx.save();
    ctx.translate(driftX, driftY);
    if (turnNow) {
      ctx.translate(x + w / 2, y + h / 2);
      ctx.rotate(turnNow * Math.PI / 2);
      ctx.translate(-(x + w / 2), -(y + h / 2));
    }

    /* The cuts, very faint. */
    ctx.strokeStyle = css('--rule', '#e2e2e2');
    ctx.lineWidth = 1;
    ctx.globalAlpha = ruleA * Math.min(1, grow * 1.6);
    ctx.strokeRect(x, y, w, h);
    sqs.forEach(function (sq, i) {
      if (grow * sqs.length > i) ctx.strokeRect(sq.x, sq.y, sq.s, sq.s);
    });

    /* The spiral, in the accent colour, brighter towards the eye. */
    ctx.strokeStyle = css('--accent-display', '#1d3a5f');
    ctx.lineWidth = dark ? 1.6 : 1.4;
    ctx.lineCap = 'round';
    sqs.forEach(function (sq, i) {
      var part = grow * sqs.length - i;
      if (part <= 0) return;
      ctx.globalAlpha = Math.min(1, (arcLow + (arcHigh - arcLow) * (i / sqs.length)) + near * 0.2);
      arc(sq, eye, part);
    });

    /* A dot in the eye. */
    if (last && grow > 0.98) {
      ctx.globalAlpha = dark ? 1 : 0.7;
      ctx.fillStyle = css('--accent-display', '#1d3a5f');
      ctx.beginPath();
      ctx.arc(eye[0], eye[1], 2.2 + near * 1.4, 0, 2 * Math.PI);
      ctx.fill();
    }

    ctx.globalAlpha = 1;
    ctx.restore();
  }

  /* One loop for everything that moves: the opening draw, the drift after
     the pointer, and the quarter turn after a click. */
  function tick() {
    var busy = false;

    if (grow < 1) { grow = Math.min(1, grow + 0.018); busy = true; }
    if (Math.abs(driftX - wantX) > 0.2 || Math.abs(driftY - wantY) > 0.2) {
      driftX += (wantX - driftX) * 0.08;
      driftY += (wantY - driftY) * 0.08;
      busy = true;
    }
    if (Math.abs(turnNow - turn) > 0.002) {
      turnNow += (turn - turnNow) * 0.09;
      busy = true;
    } else { turnNow = turn; }

    draw();

    if (busy) requestAnimationFrame(tick);
    else running = false;
  }

  function wake() {
    if (running) return;
    running = true;
    requestAnimationFrame(tick);
  }

  window.drawGolden = function () { draw(); wake(); };

  function start() {
    wrap = document.getElementById('golden');
    canvas = document.getElementById('golden-canvas');
    if (!wrap || !canvas) return;
    ctx = canvas.getContext('2d');
    draw();
    wake();

    var timer;
    window.addEventListener('resize', function () {
      clearTimeout(timer);
      timer = setTimeout(function () { draw(); wake(); }, 120);
    });

    if (!still) {
      window.addEventListener('pointermove', function (e) {
        px = e.clientX; py = e.clientY;
        /* A few pixels of drift, against the pointer, so the figure sits
           behind the text rather than following it around. */
        wantX = -((e.clientX / window.innerWidth) - 0.5) * 26;
        wantY = -((e.clientY / window.innerHeight) - 0.5) * 18;
        wake();
      }, { passive: true });

      window.addEventListener('pointerleave', function () {
        px = null; py = null; wantX = 0; wantY = 0; wake();
      });
    }

    /* Click anywhere on the page, except on something you can click, and
       the figure turns a quarter. */
    window.addEventListener('click', function (e) {
      if (e.target.closest('a, button, input, select, textarea, .home-lede')) return;
      turn += 1;
      wake();
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
