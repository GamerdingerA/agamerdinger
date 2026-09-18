/* The homepage drawing.

   phi is 1.6180339887. A rectangle whose sides stand in that ratio can be
   cut into a square and a smaller rectangle of the same shape, over and
   over. Drawing the cuts and then the quarter circle inside each square
   gives the spiral.

   Colours come from the CSS custom properties, so the drawing follows the
   theme and the accent picker without knowing anything about either.
   window.drawGolden is called by theme.js when either changes. */
(function () {
  var PHI = (1 + Math.sqrt(5)) / 2;
  var DEPTH = 11;

  function css(name, fallback) {
    var v = getComputedStyle(document.documentElement)
      .getPropertyValue(name).trim();
    return v || fallback;
  }

  /* Each step turns the rectangle a quarter turn. The square sits on the
     side given by the step number, and what is left is the next
     rectangle. Returns the list of squares, largest first. */
  function squares(x, y, w, h, n) {
    var out = [];
    var side, dir = 0;
    for (var i = 0; i < n && w > 1 && h > 1; i++) {
      if (w >= h) {
        side = h;
        if (dir % 2 === 0) { out.push({ x: x, y: y, s: side, q: dir }); x += side; }
        else { out.push({ x: x + w - side, y: y, s: side, q: dir }); }
        w -= side;
      } else {
        side = w;
        if (dir % 2 === 1) { out.push({ x: x, y: y, s: side, q: dir }); y += side; }
        else { out.push({ x: x, y: y + h - side, s: side, q: dir }); }
        h -= side;
      }
      dir = (dir + 1) % 4;
    }
    return out;
  }

  /* The quarter circle inside one square. It is centred on whichever
     corner of the square lies nearest the eye of the spiral, so the arcs
     of neighbouring squares meet end to end. */
  function arc(ctx, sq, eye) {
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
    var a1 = mx > cx ? 0 : Math.PI;              /* along the x axis */
    var a2 = my > cy ? Math.PI / 2 : -Math.PI / 2; /* along the y axis */
    var ccw = ((a2 - a1 + 2 * Math.PI) % (2 * Math.PI)) !== Math.PI / 2;

    ctx.beginPath();
    ctx.arc(cx, cy, sq.s, a1, a2, ccw);
    ctx.stroke();
  }

  function draw() {
    var wrap = document.getElementById('golden');
    var canvas = document.getElementById('golden-canvas');
    if (!wrap || !canvas) return;

    var ratio = Math.min(window.devicePixelRatio || 1, 2);
    var W = wrap.clientWidth, H = wrap.clientHeight;
    canvas.width = W * ratio;
    canvas.height = H * ratio;
    canvas.style.width = W + 'px';
    canvas.style.height = H + 'px';

    var ctx = canvas.getContext('2d');
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    ctx.clearRect(0, 0, W, H);

    /* The figure sits in the right-hand part of the window, clear of the
       text, and always whole. Its height is what the window allows; the
       width follows from it. */
    var maxW = W * (W < 700 ? 0.86 : 0.48);
    var maxH = H * 0.78;
    var h = Math.min(maxH, maxW / PHI);
    var w = h * PHI;
    var x = W < 700 ? (W - w) / 2 : W - w - Math.min(W * 0.04, 48);
    var y = (H - h) / 2;

    var sqs = squares(x, y, w, h, DEPTH);

    /* The cuts, very faint. */
    ctx.strokeStyle = css('--rule', '#e2e2e2');
    ctx.lineWidth = 1;
    ctx.globalAlpha = 0.38;
    ctx.strokeRect(x, y, w, h);
    sqs.forEach(function (sq) { ctx.strokeRect(sq.x, sq.y, sq.s, sq.s); });

    /* The spiral itself, in the accent colour, fading in towards the eye. */
    var last = sqs[sqs.length - 1];
    var eye = last ? [last.x + last.s / 2, last.y + last.s / 2] : [x, y];

    ctx.strokeStyle = css('--accent-display', '#1d3a5f');
    ctx.lineWidth = 1.4;
    ctx.lineCap = 'round';
    sqs.forEach(function (sq, i) {
      ctx.globalAlpha = 0.12 + 0.34 * (i / sqs.length);
      arc(ctx, sq, eye);
    });

    /* A dot in the eye of the spiral. */
    if (last) {
      ctx.globalAlpha = 0.7;
      ctx.fillStyle = css('--accent-display', '#1d3a5f');
      ctx.beginPath();
      ctx.arc(last.x + last.s / 2, last.y + last.s / 2, 2.2, 0, 2 * Math.PI);
      ctx.fill();
    }
    ctx.globalAlpha = 1;
  }

  window.drawGolden = draw;

  var timer;
  window.addEventListener('resize', function () {
    clearTimeout(timer);
    timer = setTimeout(draw, 120);
  });

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', draw);
  } else {
    draw();
  }
})();
