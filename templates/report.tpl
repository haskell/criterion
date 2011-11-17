<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>criterion report</title>
    <!--[if lte IE 8]>
      <script language="javascript" type="text/javascript"
              src="file://{{jspath}}/excanvas-r3.min.js"></script>
    <![endif]-->
    <script language="javascript" type="text/javascript"
            src="file://{{jspath}}/jquery-1.6.4.min.js"></script>
    <script language="javascript" type="text/javascript"
	    src="file://{{jspath}}/jquery.flot-0.7.min.js"></script>
    <script language="javascript" type="text/javascript"
	    src="file://{{jspath}}/jquery.criterion.js"></script>
    <style type="text/css">
@import url("file://{{tplpath}}/criterion.css");
</style>
 </head>
    <body>
      <div class="body">
    <h1>criterion performance measurements</h1>

<ul>
{{#report}}
<li><a href="#b{{number}}">{{name}}</a></li>
{{/report}}
</ul>

{{#report}}
<h2><a name="b{{number}}">{{name}}</a></h2>
 <table width="100%">
  <tbody>
   <tr>
    <td><div id="kde{{number}}" class="kdechart"
	     style="width:450px;height:300px;"></div></td>
    <td><div id="time{{number}}" class="timechart"
	     style="width:450px;height:300px;"></div></td>
  </tbody>
 </table>
 <table>
  <thead class="analysis">
   <th></th>
   <th class="cibound">lower bound</th>
   <th>estimate</th>
   <th class="cibound">upper bound</th>
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

{{/report}}

<script type="text/javascript">
$(function () {
  function mangulate(number, name, times, kdetimes, kdepdf) {
    kdetimes = $.scaleTimes(kdetimes)[0];
    var ts = $.scaleTimes(times);
    var units = ts[1];
    ts = ts[0];
    $.plot($("#kde" + number),
	   [{ label: name + " time densities (" + units + ")",
	      data: $.zip(kdetimes, kdepdf),
	      }],
	   { yaxis: { ticks: false },
	     grid: { hoverable: true },
	   });
    var timepairs = new Array(ts.length);
    for (var i = 0; i < ts.length; i++)
      timepairs[i] = [ts[i],i];
    $.plot($("#time" + number),
	   [{ label: name + " times (" + units + ")",
	      data: timepairs }],
	   { points: { show: true },
	     grid: { hoverable: true },
             xaxis: { min: kdetimes[0], max: kdetimes[kdetimes.length-1] },
	     yaxis: { ticks: false },
	   });
    $.addTooltip("#kde" + number, function(x,y) { return x + ' ' + units; });
    $.addTooltip("#time" + number, function(x,y) { return x + ' ' + units; });
  };
  {{#report}}
  mangulate({{number}}, "{{name}}",
	    {{times}},
	    {{kdetimes}},
            {{kdepdf}});
  {{/report}}
});
$(document).ready(function () {
    $(".time").text(function(_, text) {
        return $.renderTime(text);
      });
    $(".citime").text(function(_, text) {
        return $.renderTime(text);
      });
  });
</script>

   </div>
 </body>
</html>
