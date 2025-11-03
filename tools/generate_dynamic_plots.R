library(tidyverse)
library(plotly)
library(scales)
library(htmlwidgets)


#' Generate a Plotly graphic that dynamically updates the Total line when data is removed by the user
#' 
#' @param data A data frame containing values for each category, including a placeholder column for the total.
#' @param trace_order A vector of column values from `data`, specifying the order they will be listed in the legend.
#' @param trace_labels A vector of category names that will appear in the legend.
#' @param colors A vector of colors, corresponding to the `trace_order`.
#' @param time_unit Duration represented by each data value, as specified by the column name in `data`; one of ("month", "quarter").
#' @return A `plot_ly` object displaying data for each category, along with their total.
dynamic_total_plot <- function(data, trace_order, trace_labels, colors, time_unit) {
  # Define trace colors
  trace_colors <- setNames(colors, trace_labels)
  
  # Build plot_ly object
  p <- plot_ly(data, x = data[[time_unit]])
  for (i in seq_along(trace_order)) {
    var <- trace_order[i]
    label <- trace_labels[i]
    p <- add_trace(p, y = data[[var]], name = label,
                   type = "scatter", mode = "lines", line = list(color = trace_colors[[label]]),
                   text = paste0("<b>", label, "</b><br><i>", format(total_costs$month, "%B %Y"),
                                 "</i><br>Cost: ", dollar(total_costs[[var]])),
                   hoverinfo = "text")
  }
  
  # Add dummy Total trace
  initial_total <- total_costs$cc + total_costs$utils + total_costs$rent
  p <- add_trace(p, y = initial_total, name = "Total",
                 type = "scatter", mode = "lines", line = list(color = "black", width = 2),
                 text = paste0("<b>Total</b><br><i>", format(total_costs$month, "%B %Y"), 
                               "</i><br>Cost: ", dollar(initial_total)),
                 hoverinfo = "text")
  
  # Add JavaScript callback to calculate Total dynamically
  p <- onRender(p, "
  function(el, x) {
    var Plotly = window.Plotly;
  
    function updateTotal() {
      var gd = document.getElementById(el.id);
      var data = gd.data;
      var totalY = Array(data[0].y.length).fill(0);
  
      for (var i = 0; i < data.length - 1; i++) {
        var trace = data[i];
        var visible = trace.visible;
        if (visible === true || visible === undefined) {
          for (var j = 0; j < trace.y.length; j++) {
            totalY[j] += trace.y[j];
          }
        }
      }
  
      // Update Total trace
      Plotly.restyle(gd, { y: [totalY] }, [data.length - 1]);
  
      // Update hover text
      var months = data[0].x;
      var newText = months.map(function(m, i) {
        return '<b>Total</b><br><i>' + m + '</i><br>Cost: $' + totalY[i].toLocaleString();
      });
      Plotly.restyle(gd, { text: [newText] }, [data.length - 1]);
    }
  
    // Recalculate after any plot update
    el.on('plotly_afterplot', updateTotal);
  }
  ")
  
  return(p)
}
