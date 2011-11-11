(function ($) {
  $.zip = function(a,b) {
    var x = Math.min(a.length,b.length);
    var c = new Array(x);
    for (var i = 0; i < x; i++)
      c[i] = [a[i],b[i]];
    return c;
  };

  $.mean = function(ary) {
    var m = 0, i = 0;

    while (i < ary.length) {
      var j = i++;
      m += (ary[j] - m) / i;
    }

    return m;
  };

  $.timeUnits = function(secs) {
    if (secs < 0)           return timeUnits(-secs);
    else if (secs >= 1e9)   return [1e-9, "Gs"];
    else if (secs >= 1e6)   return [1e-6, "Ms"];
    else if (secs >= 1)     return [1,    "s"];
    else if (secs >= 1e-3)  return [1e3,  "ms"];
    else if (secs >= 1e-6)  return [1e6,  "\u03bcs"];
    else if (secs >= 1e-9)  return [1e9,  "ns"];
    else if (secs >= 1e-12) return [1e12, "ps"];
  };

  $.scaleTimes = function(ary) {
    var s = $.timeUnits($.mean(ary));
    var nary = new Array(ary.length);
    for (var i = 0; i < ary.length; i++)
      nary[i] = ary[i] * s[0];
    return [nary, s[1]];
  };

  $.renderTime = function(text) {
    var x = parseFloat(text);
    var t = $.timeUnits(x);
    x *= t[0];
    if (x >= 1000 || x <= -1000) return x.toFixed() + " " + t[1];
    var prec = 5;
    if (x < 0) prec++;

    return x.toString().substring(0,prec) + " " + t[1];
  };
})(jQuery);
