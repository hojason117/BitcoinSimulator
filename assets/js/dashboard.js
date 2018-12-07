export function renderTxChart() {
  var ctx = document.getElementById("txChart").getContext('2d');
  return new Chart(ctx, {
    type: 'line',
    data: {
      labels: ["1", "2", "3", "4"],
      datasets: [{
        label: 'Frequency',
        data: [3, 5, 4, 8],
        backgroundColor: 'rgba(250, 175, 50, 0.2)',
        borderColor: 'rgba(250, 175, 50, 1)',
        borderWidth: 1
      }]
    },
    options: {
      title: {
        display: true,
        fontSize: 25,
        text: "Transaction"
      },
      scales: {
        xAxes: [{
          gridLines: {
            display: false
          }
        }],
        yAxes: [{
          scaleLabel: {
            display: true,
            labelString: "Transactions Per Minute"
          },
          ticks: {
            beginAtZero: true
          }
        }]
      }
    }
  });
}

export function renderMiningChart() {
  var ctx = document.getElementById("miningChart").getContext('2d');
  return new Chart(ctx, {
    type: 'line',
    data: {
      labels: ["1", "2", "3", "4"],
      datasets: [{
          label: 'Frequency',
          data: [3, 5, 4, 8],
          backgroundColor: 'rgba(80, 220, 100, 0.2)',
          borderColor: 'rgba(80, 220, 100, 1)',
          borderWidth: 1
        }
      ]
    },
    options: {
      title: {
        display: true,
        fontSize: 25,
        text: "Mining"
      },
      scales: {
        xAxes: [{
          gridLines: {
            display: false
          }
        }],
        yAxes: [{
          scaleLabel: {
            display: true,
            labelString: "Blocks Per Minute"
          },
          ticks: {
            beginAtZero: true
          }
        }]
      }
    }
  });
}
