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
  function showTooltip(x, y, contents) {
      $('<div id="tooltip">' + contents + '</div>').css( {
	  position: 'absolute',
	  display: 'none',
	  top: y + 5,
	  left: x + 5,
	  border: '1px solid #fdd',
	  padding: '2px',
	  'background-color': '#fee',
	  opacity: 0.80
      }).appendTo("body").fadeIn(200);
  };
  {{#report}}
  var kdetimes{{number}} = $.scaleTimes({{kdetimes}})[0];
  var kdepdf{{number}} = {{kdepdf}};
  var ts{{number}} = $.scaleTimes({{times}});
  var units{{number}} = ts{{number}}[1];
  ts{{number}} = ts{{number}}[0];
  $.plot($("#kde{{number}}"),
         [{ label: "{{name}} time densities (" + units{{number}} + ")",
            data: $.zip(kdetimes{{number}},kdepdf{{number}}),
            }],
         { yaxis: { ticks: false },
           grid: { hoverable: true },
         });
  var times{{number}} = new Array(ts{{number}}.length);
  for (var i = 0; i < ts{{number}}.length; i++)
    times{{number}}[i] = [i,ts{{number}}[i]];
  $.plot($("#time{{number}}"),
         [{ label: "{{name}} times (" + units{{number}} + ")",
            data: times{{number}} }],
         { points: { show: true },
           grid: { hoverable: true },
           xaxis: { ticks: false },
           yaxis: { min: 0 },
         });
    var pp{{number}} = null;
    $("#time{{number}}").bind("plothover", function (event, pos, item) {
        $("#x").text(pos.x.toFixed(2));
        $("#y").text(pos.y.toFixed(2));

	if (item) {
	    if (pp{{number}} != item.dataIndex) {
		pp{{number}} = item.dataIndex;

		$("#tooltip").remove();
		var x = item.datapoint[0].toFixed(2),
		    y = item.datapoint[1].toFixed(2);

		showTooltip(item.pageX, item.pageY, y + " " + units{{number}});
	    }
	}
	else {
	    $("#tooltip").remove();
	    pp{{number}} = null;            
	}
    });
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
