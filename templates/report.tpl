<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>criterion report</title>
    <!--[if lte IE 8]>
      <script language="javascript" type="text/javascript">
        {{#include}}js/excanvas-r3.min.js{{/include}}
      </script>
    <![endif]-->
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery-1.6.4.min.js{{/include}}
    </script>
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery.flot-0.7.min.js{{/include}}
    </script>
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery.criterion.js{{/include}}
    </script>
    <style type="text/css">
{{#include}}criterion.css{{/include}}
    </style>
    <!--[if !IE 7]>
	    <style type="text/css">
		    #wrap {display:table;height:100%}
	    </style>
    <![endif]-->
 </head>
    <body>
     <div id="wrap">
      <div id="main" class="body">
    <h1>criterion performance measurements</h1>

<h2>overview</h2>

<p><a href="#grokularation">want to understand this report?</a></p>

<div id="overview" class="ovchart" style="width:900px;height:100px;"></div>

{{#report}}
<h2><a name="b{{number}}">{{name}}</a></h2>
 <table width="100%">
  <tbody>
   <tr>
    <td><div id="kde{{number}}" class="kdechart"
             style="width:450px;height:278px;"></div></td>
    <td><div id="time{{number}}" class="timechart"
             style="width:450px;height:278px;"></div></td>
   </tr>
  </tbody>
 </table>
 <table>
  <thead class="analysis">
   <th></th>
   <th class="cibound"
       title="{{anMean.estConfidenceLevel}} confidence level">lower bound</th>
   <th>estimate</th>
   <th class="cibound"
       title="{{anMean.estConfidenceLevel}} confidence level">upper bound</th>
  </thead>
  <tbody>
   <tr>
    <td>Mean execution time</td>
    <td><span class="citime">{{anMean.estLowerBound}}</span></td>
    <td><span class="time">{{anMean.estPoint}}</span></td>
    <td><span class="citime">{{anMean.estUpperBound}}</span></td>
   </tr>
   <tr>
    <td>Standard deviation</td>
    <td><span class="citime">{{anStdDev.estLowerBound}}</span></td>
    <td><span class="time">{{anStdDev.estPoint}}</span></td>
    <td><span class="citime">{{anStdDev.estUpperBound}}</span></td>
   </tr>
  </tbody>
 </table>

 <span class="outliers">
   <p>Outlying measurements have {{anOutlierVar.ovDesc}}
     (<span class="percent">{{anOutlierVar.ovFraction}}</span>%)
     effect on estimated standard deviation.</p>
 </span>
{{/report}}

 <h2><a name="grokularation">understanding this report</a></h2>

 <p>In this report, each function benchmarked by criterion is assigned
   a section of its own.  In each section, we display two charts, each
   with an <i>x</i> axis that represents measured execution time.
   These charts are active; if you hover your mouse over data points
   and annotations, you will see more details.</p>

 <ul>
   <li>The chart on the left is a
     <a href="http://en.wikipedia.org/wiki/Kernel_density_estimation">kernel
       density estimate</a> (also known as a KDE) of time
     measurements.  This graphs the probability of any given time
     measurement occurring.  A spike indicates that a measurement of a
     particular time occurred; its height indicates how often that
     measurement was repeated.</li>

   <li>The chart on the right is the raw data from which the kernel
     density estimate is built.  Measurements are displayed on
     the <i>y</i> axis in the order in which they occurred.</li>
 </ul>
   
 <p>Under the charts is a small table displaying the mean and standard
   deviation of the measurements.  We use a statistical technique
   called
   the <a href="http://en.wikipedia.org/wiki/Bootstrapping_(statistics)">bootstrap</a>
   to provide confidence intervals on our estimates of these values.
   The bootstrap-derived upper and lower bounds on the mean and
   standard deviation let you see how accurate we believe those
   estimates to be.  (Hover the mouse over the table headers to see
   the confidence levels.)</p>
   
 <p>A noisy benchmarking environment can cause some or many
   measurements to fall far from the mean.  These outlying
   measurements can have a significant inflationary effect on the
   estimate of the standard deviation.  We calculate and display an
   estimate of the extent to which the standard deviation has been
   inflated by outliers.</p>

<script type="text/javascript">
$(function () {
  function mangulate(number, name, mean, times, kdetimes, kdepdf) {
    var meanSecs = mean;
    var units = $.timeUnits(mean);
    var scale = units[0];
    units = units[1];
    mean *= scale;
    kdetimes = $.scaleBy(scale, kdetimes);
    var ts = $.scaleBy(scale, times);
    var kq = $("#kde" + number);
    var k = $.plot(kq,
           [{ label: name + " time densities",
              data: $.zip(kdetimes, kdepdf),
              }],
           { xaxis: { tickFormatter: $.unitFormatter(units) },
             yaxis: { ticks: false },
             grid: { borderColor: "#777",
                     hoverable: true, markings: [ { color: '#6fd3fb',
                     lineWidth: 1.5, xaxis: { from: mean, to: mean } } ] },
           });
    var o = k.pointOffset({ x: mean, y: 0});
    kq.append('<div class="meanlegend" title="' + $.renderTime(meanSecs) +
              '" style="position:absolute;left:' + (o.left + 4) +
              'px;bottom:139px;">mean</div>');
    var timepairs = new Array(ts.length);
    for (var i = 0; i < ts.length; i++)
      timepairs[i] = [ts[i],i];
    $.plot($("#time" + number),
           [{ label: name + " times",
              data: timepairs }],
           { points: { show: true },
             grid: { borderColor: "#777", hoverable: true },
             xaxis: { min: kdetimes[0], max: kdetimes[kdetimes.length-1],
                      tickFormatter: $.unitFormatter(units) },
             yaxis: { ticks: false },
           });
    $.addTooltip("#kde" + number, function(x,y) { return x + ' ' + units; });
    $.addTooltip("#time" + number, function(x,y) { return x + ' ' + units; });
  };
  {{#report}}
  mangulate({{number}}, "{{name}}",
            {{anMean.estPoint}},
            [{{#times}}{{x}},{{/times}}],
            [{{#kdetimes}}{{x}},{{/kdetimes}}],
            [{{#kdepdf}}{{x}},{{/kdepdf}}]);
  {{/report}}

  var benches = [{{#report}}"{{name}}",{{/report}}];
  var ylabels = [{{#report}}[-{{number}},'<a href="#b{{number}}">{{name}}</a>'],{{/report}}];
  var means = $.scaleTimes([{{#report}}{{anMean.estPoint}},{{/report}}]);
  var xs = [];
  var prev = null;
  for (var i = 0; i < means[0].length; i++) {
    var name = benches[i].split(/\//);
    name.pop();
    name = name.join('/');
    if (name != prev) {
      xs.push({ label: name, data: [[means[0][i], -i]]});
      prev = name;
    }
    else
      xs[xs.length-1].data.push([means[0][i],-i]);
  }
  var oq = $("#overview");
  o = $.plot(oq, xs, { bars: { show: true, horizontal: true,
                               barWidth: 0.75, align: "center" },
                       grid: { borderColor: "#777", hoverable: true },
                       legend: { show: xs.length > 1 },
                       xaxis: { max: Math.max.apply(undefined,means[0]) * 1.02 },
                       yaxis: { ticks: ylabels, tickColor: '#ffffff' } });
  if (benches.length > 3)
    o.getPlaceholder().height(28*benches.length);
  o.resize();
  o.setupGrid();
  o.draw();
  $.addTooltip("#overview", function(x,y) { return x + ' ' + means[1]; });
});
$(document).ready(function () {
    $(".time").text(function(_, text) {
        return $.renderTime(text);
      });
    $(".citime").text(function(_, text) {
        return $.renderTime(text);
      });
    $(".percent").text(function(_, text) {
        return (text*100).toFixed(1);
      });
  });
</script>

   </div>
  </div>
  <div id="footer">
    <div class="body">
     <div class="footfirst">
      <h2>colophon</h2>
      <p>This report was created using the
	<a href="http://hackage.haskell.org/package/criterion">criterion</a>
	benchmark execution and performance analysis tool.</p>
      <p>Criterion is developed and maintained
      by <a href="http://www.serpentine.com/blog/">Bryan O'Sullivan</a>.</p>
     </div>
    </div>
  </div>
 </body>
</html>
