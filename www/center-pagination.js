$(document).on('shiny:connected', function() {
  $(document).on('draw.dt', function() {
    $('div.dataTables_paginate ul.pagination').css({
      'display': 'flex',
      'justify-content': 'center',
      'width': '100%',
      'margin': '0.5rem',
      'padding': '0'
    });
  });
});
