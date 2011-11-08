<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>criterion report</title>
    <!--[if lte IE 8]><script language="javascript" type="text/javascript" src="http://people.iola.dk/olau/flot/excanvas.min.js"></script><![endif]-->
    <script language="javascript" type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
    <script language="javascript" type="text/javascript" src="http://people.iola.dk/olau/flot/jquery.flot.js"></script>
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
    Ran in {{anMean.estPoint}}.
    <div id="time{{number}}" style="width:600px;height:300px;"></div>
{{/report}}

<script type="text/javascript">
$(function () {
  {{#report}}
  var kde{{number}} = {{kde}};
  $.plot($("#kde{{number}}"), [kde{{number}}], { yaxis: { ticks: false } });
  var ts{{number}} = {{times}};
  var times{{number}} = [];
  for (var i = 0; i < ts{{number}}.length; i++)
    times{{number}}.push([i,ts{{number}}[i]]);
  $.plot($("#time{{number}}"), [times{{number}}], { points: { show: true } });
  {{/report}}
});
</script>

 </body>
</html>
