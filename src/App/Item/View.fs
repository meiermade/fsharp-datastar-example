module App.Item.View

open App
open App.Common.View
open App.Item.Model
open FSharp.ViewEngine
open type Html

let createChartData (items:Item seq) =
    items
    |> Seq.sortBy _.id
    |> Seq.map (fun item -> [| item.x; item.y |])
    |> Seq.toArray
    
let itemsChart (items:Item seq) =
    let chartData = createChartData items
    let signals = {| _itemsChartData = chartData |}
    let signalsJs = Js.serialize signals
    div [
        _id "item-chart-container"
        _data ("signals", signalsJs)
        _data ("init", "renderChart($_itemsChartData)")
        _data ("on-signal-patch", "renderChart($_itemsChartData)")
        _data ("on-signal-patch-filter", "/^_itemsChartData$/")
        _children [
            div [
                _id "item-chart"
                _data ("on:chart-clicked", "$x = event.detail.x; $y = event.detail.y; @post('/items')")
                _class "w-full h-96 mb-4"
            ]
            // language=javascript
            script """
            var chartDom = document.getElementById('item-chart');
            var myChart = echarts.init(chartDom, {renderer: 'svg'});
            myChart.getZr().on('click', function(params) {
                const pointInPixel = [params.offsetX, params.offsetY];
                const pointInGrid = myChart.convertFromPixel({ seriesIndex: 0 }, pointInPixel);
                // check if click was inside plot area
                const [x, y] = pointInGrid;
                if (!isNaN(x) && !isNaN(y)) {
                    chartDom.dispatchEvent(new CustomEvent('chart-clicked', {
                        detail: { x: Math.round(x), y: Math.round(y) }
                    }));
                }
            });

            function renderChart(data) {
                var options = {
                    xAxis: {
                        type: 'value',
                        name: 'X Value',
                        minInterval: 1,
                        min: 0,
                        max: 10
                    },
                    yAxis: {
                        type: 'value',
                        name: 'Y Value',
                        minInterval: 1,
                        min: 0,
                        max: 10
                    },
                    tooltip: {
                        trigger: 'item', // show on point hover
                        formatter: (params) => {
                            const [x, y] = params.value;
                            return `(${x},${y})`;
                        }
                    },
                    series: [{
                        type: 'scatter',
                        data: data,
                        universalTransition: false,
                        animationDurationUpdate: (idx) => {
                            return idx === data.length - 1 ? 500 : 0;
                        },
                        animationDelayUpdate: (idx) => {
                            return idx === data.length - 1 ? 0 : 0;
                        }
                    }]
                };
                myChart.setOption(options);
            }
            """
            pre [
                _data "json-signals"
                _class "bg-gray-100 p-4 rounded-md overflow-x-auto max-h-64"
            ]
        ]
    ]
    
let itemsPage (items:Item seq) =
    div [
        _id "page"
        _data ("init", "$selectedNav = 'items'; window.history.replaceState({}, '', '/items')")
        _data ("on:item-created__window", "@get('/items/data')")
        _children [
            h1 [
                _class "text-center text-3xl font-bold mb-4"
                _children "Items"
            ]
            form [
                _id "create-item-form"
                _data ("on:submit", "@post('/items')")
                _class "w-full border border-gray-200 p-4 rounded-md bg-white"
                _children [
                    p [ _class "mb-4 font-bold"; _children "Create Item" ]
                    div [
                        _class "mb-4 grid grid-cols-1 gap-4 sm:grid-cols-3 "
                        _children [
                            numberField ("x", "x value", 0, 10)
                            numberField ("y", "y value", 0, 10)
                        ]
                    ]
                    div [
                        _class "w-full flex gap-4"
                        _children [
                            button [
                                _type "submit"
                                _class "btn btn-sm btn-primary"
                                _children "Create"
                            ]
                            button [
                                _type "button"
                                _class "btn btn-sm btn-secondary"
                                _data ("on:click", "$x = Math.floor(Math.random() * 11); $y = Math.floor(Math.random() * 11); @post('/items')")
                                _children "Random"
                            ]
                        ]
                    ]
                ]
            ]
            itemsChart items
        ]
    ]
