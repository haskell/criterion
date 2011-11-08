<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>Kernel density</title>
    <!--[if lte IE 8]><script language="javascript" type="text/javascript" src="http://people.iola.dk/olau/flot/excanvas.min.js"></script><![endif]-->
    <script language="javascript" type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
    <script language="javascript" type="text/javascript" src="http://people.iola.dk/olau/flot/jquery.flot.js"></script>
 </head>
    <body>
    <h1>Kernel density</h1>

{{#report}}
    <div id="kde{{number}}" style="width:600px;height:300px;"></div>
    <div id="time{{number}}" style="width:600px;height:300px;"></div>
{{/report}}

    <p>Simple example. You don't need to specify much to get an
       attractive look. Put in a placeholder, make sure you set its
       dimensions (otherwise the plot library will barf) and call the
       plot function with the data. The axes are automatically
       scaled.</p>

<script type="text/javascript">
$(function () {
    {{#report}}
    var kde = {{kde}};
    $.plot($("#kde{{number}}"), [kde], { yaxis: { ticks: false } });
    var ts = {{times}};
    var times = [];
    for (var i = 0; i < ts.length; i++)
      times.push([i,ts[i]]);
    $.plot($("#time{{number}}"), [times], { points: { show: true } });
    {{/report}}
});
</script>

 </body>
</html>
