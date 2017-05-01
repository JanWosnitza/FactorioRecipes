module FactorioRecipes.SankeyHtml

open System.IO

let private a = """
<html>
  <head>
    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
    <script type="text/javascript">
	google.charts.load('current', {
	'packages': ['sankey']
	});
	google.charts.setOnLoadCallback(drawChart);

	function drawChart() {
	var data = new google.visualization.DataTable();
	data.addColumn('string', 'From');
	data.addColumn('string', 'To');
	data.addColumn('number', 'Weight');
	data.addColumn({type:'string', role:'tooltip'});
	data.addRows([
"""

let private b =
    """
	]);

	// Sets chart options.
	var options = {
		sankey: {
			node: {
				width: 15,
				color: { stroke: 'gray', strokeWidth: 1 },
				nodePadding: 20,
				label: { fontSize: 16, bold: true },
				interactivity: true
			},
			link: {
				colorMode: "source",
				color: { stroke: '#777', strokeWidth: 1.5 }
			},
			iterations: 100
		},
	};

	// Instantiates and draws our chart, passing in some options.
	var chart = new google.visualization.Sankey(document.getElementById('sankey_basic'));
	chart.draw(data, options);
	}
    </script>
  </head>
  <body>
    <div id="sankey_basic" style="width: 100%; height: 100%;"></div>
  </body>
</html>
"""

let get (list) =
    let l =
        list
        |> Seq.map (fun (f, t, a, tooltip) -> sprintf "        ['%s', '%s', %f, '%s']" f t a tooltip)
        |> Seq.reduce (sprintf "%s,\n%s")
    a + l + b

let safe (fileName) (list) =
    let file = Path.GetFullPath( fileName + ".html" )
    File.WriteAllText( file, get list )
    file
