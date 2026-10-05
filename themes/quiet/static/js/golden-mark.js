/* The homepage spiral, small and faint, in the right margin of every other
   page. It is the same figure, so the site reads as one drawing seen from
   different places.

   Two things carry meaning. Each section of the site turns the figure a
   further quarter, in menu order (publications, projects, research agenda,
   talks, about), so moving through the menu walks round the spiral. And
   the spiral draws itself as you read: square by square, down to the eye,
   which it reaches at the end of the page. A page too short to scroll
   draws itself through once, slowly.

   Hidden where the margin is too narrow to hold it. */
(function () {
  var PHI = (1 + Math.sqrt(5)) / 2;
  var DEPTH = 12;
  var LIGHT_BLUE = '#c9e0ff';

  var still = window.matchMedia &&
    window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  var wrap, canvas, ctx, turn = 0;
  var grow = 0, want = 0, running = false;

  function css(name, fallback) {
    var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
    return v || fallback;
  }

  function isDark() {
    if (window.SiteTheme && window.SiteTheme.isDark) return window.SiteTheme.isDark();
    return !!(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
  }

  /* Same cut as the homepage: square, smaller golden rectangle, repeat. */
  function squares(x, y, w, h, n) {
    var out = [], side, dir = 0;
    for (var i = 0; i < n && w > 0.6 && h > 0.6; i++) {
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

  function arc(sq, eye, part) {
    var corners = [[sq.x, sq.y], [sq.x + sq.s, sq.y], [sq.x, sq.y + sq.s], [sq.x + sq.s, sq.y + sq.s]];
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
    ctx.beginPath();
    ctx.arc(cx, cy, sq.s, a1, ccw ? a1 - sweep : a1 + sweep, ccw);
    ctx.stroke();
  }

  /* Width of the empty margin to the right of the text column. The right
     side keeps it clear of the filter controls and the overview list. */
  function margin() {
    var col = document.querySelector('main.shell, main .shell, main');
    if (!col) return 0;
    var r = col.getBoundingClientRect();
    var pad = parseFloat(getComputedStyle(col).paddingRight) || 0;
    return window.innerWidth - (r.right - pad);
  }

  function draw() {
    var W = window.innerWidth, H = window.innerHeight;
    var room = margin() - 40;           /* keep clear of the text */
    var odd = turn % 2 === 1;

    /* The figure's footprint on screen, before turning: long side across
       for even turns, standing up for odd ones. */
    var across = Math.min(room, odd ? H * 0.42 / PHI : 300);
    if (across < 110) { wrap.hidden = true; return; }
    wrap.hidden = false;

    var w = odd ? across * PHI : across;   /* long side of the rectangle */
    var h = w / PHI;
    var boxW = odd ? h : w, boxH = odd ? w : h;

    var ratio = Math.min(window.devicePixelRatio || 1, 2);
    if (canvas.width !== Math.round(W * ratio) || canvas.height !== Math.round(H * ratio)) {
      canvas.width = Math.round(W * ratio); canvas.height = Math.round(H * ratio);
      canvas.style.width = W + 'px'; canvas.style.height = H + 'px';
    }
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    ctx.clearRect(0, 0, W, H);

    /* Centred in the right margin, near the foot of the window. */
    var m = margin();
    var left = W - m + Math.max(8, (m - boxW) / 2 + 8);
    if (left + boxW > W - 24) left = W - 24 - boxW;
    var cx = left + boxW / 2;
    var cy = H - 40 - boxH / 2;

    ctx.save();
    ctx.translate(cx, cy);
    ctx.rotate(turn * Math.PI / 2);
    ctx.translate(-w / 2, -h / 2);

    var sqs = squares(0, 0, w, h, DEPTH);
    var last = sqs[sqs.length - 1];
    var eye = last ? [last.x + last.s / 2, last.y + last.s / 2] : [0, 0];
    var dark = isDark();
    var ink = dark ? LIGHT_BLUE : css('--accent-display', '#1d3a5f');
    var n = grow * sqs.length;

    /* The cuts, barely there. */
    ctx.lineWidth = 1;
    ctx.strokeStyle = dark ? LIGHT_BLUE : css('--rule', '#e2e2e2');
    ctx.globalAlpha = dark ? 0.15 : 0.7;
    sqs.forEach(function (sq, i) { if (n > i) ctx.strokeRect(sq.x, sq.y, sq.s, sq.s); });

    /* The arc, a touch stronger towards the eye. */
    ctx.strokeStyle = ink;
    ctx.lineWidth = 1.1;
    ctx.lineCap = 'round';
    sqs.forEach(function (sq, i) {
      var part = n - i;
      if (part <= 0) return;
      ctx.globalAlpha = (dark ? 0.26 : 0.18) + (dark ? 0.3 : 0.22) * (i / sqs.length);
      arc(sq, eye, part);
    });

    if (last && grow > 0.995) {
      ctx.globalAlpha = dark ? 0.8 : 0.5;
      ctx.fillStyle = ink;
      ctx.beginPath();
      ctx.arc(eye[0], eye[1], 1.8, 0, 2 * Math.PI);
      ctx.fill();
    }
    ctx.restore();
    ctx.globalAlpha = 1;
  }

  /* How far down the page the reader is, 0 to 1. A page that cannot
     scroll counts as read. */
  function progress() {
    var max = document.documentElement.scrollHeight - window.innerHeight;
    if (max < 40) return 1;
    return Math.max(0, Math.min(1, window.scrollY / max));
  }

  function target() {
    /* A first square or so is always there, so the mark is never blank. */
    return still ? 1 : Math.max(0.12, progress());
  }

  function tick() {
    var d = want - grow;
    if (Math.abs(d) > 0.001) {
      /* Slow enough to watch it being built: at most a third of a square
         per frame. */
      grow += Math.sign(d) * Math.min(Math.abs(d), 0.0035, Math.abs(d) * 0.06 + 0.0008);
      draw();
      requestAnimationFrame(tick);
    } else {
      grow = want; draw(); running = false;
    }
  }

  function wake() {
    want = target();
    if (running) return;
    running = true;
    requestAnimationFrame(tick);
  }

  window.drawGolden = function () { draw(); };

  function start() {
    wrap = document.getElementById('golden-mark');
    if (!wrap) return;
    canvas = wrap.querySelector('canvas');
    ctx = canvas.getContext('2d');
    turn = parseInt(wrap.getAttribute('data-turn'), 10) || 0;
    if (still) grow = 1;
    draw();
    wake();

    window.addEventListener('scroll', wake, { passive: true });
    var timer;
    window.addEventListener('resize', function () {
      clearTimeout(timer);
      timer = setTimeout(function () { draw(); wake(); }, 120);
    });
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
