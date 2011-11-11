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
 </head>
    <body>
    <h1>criterion performance measurements</h1>

<ul>
{{#report}}
<li><a href="#b{{number}}">{{name}}</a></li>
{{/report}}
</ul>

{{#report}}
<h2><a name="b{{number}}">{{name}}</a></h2>
    <div id="kde{{number}}" style="width:600px;height:300px;"></div>
    Ran in <span class="time">{{anMean.estPoint}}</span>.
    <div id="time{{number}}" style="width:600px;height:300px;"></div>
{{/report}}

<script type="text/javascript">
$(function () {
  {{#report}}
  var kdetimes{{number}} = $.scaleTimes({{kdetimes}})[0];
  var kdepdf{{number}} = {{kdepdf}};
  $.plot($("#kde{{number}}"), [$.zip(kdetimes{{number}},kdepdf{{number}})],
         { yaxis: { ticks: false } });
  var ts{{number}} = $.scaleTimes({{times}})[0];
  var times{{number}} = new Array(ts{{number}}.length);
  for (var i = 0; i < ts{{number}}.length; i++)
    times{{number}}[i] = [i,ts{{number}}[i]];
  $.plot($("#time{{number}}"), [times{{number}}], { points: { show: true } });
  {{/report}}
});
$(document).ready(function () {
    $(".time").text(function(_, text) {
        return $.renderTime(text);
      });
  });
</script>

 </body>
</html>
