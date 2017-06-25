function CMM() {}

CMM.reports_visible = true;

// -----------------------------------------------------------------------------
// Authentication and login.
// -----------------------------------------------------------------------------

// Send AJAX login form.
CMM.dologin = function()
{
    var params = { user: $('#user').val(),
                   pass: $('#pass').val(),
                   persist: $('#persist').val() };

    $.post('/login', params, function(json) { CMM.donelogin(json); }, 'json');
}

// Handle results of login attempt, persisted login or successful signup.
CMM.donelogin = function(json)
{
    if (json.user)
    {
        CMM.user = json.user;

        if (json.month)
            CMM.month = json.month;

        if (json.year)
            CMM.year = json.year;

        $('#showlogin').text('Logout');
        $('#showlogin').unbind('click', CMM.togglelogin).click(CMM.dologout);

        $('#loginform').hide('slow', CMM.doneloginOK);
    }
    else
    {
        $('#login_message').addClass('Red').text(json.fail);
    }
}

// Update UI for a successful login.
CMM.doneloginOK = function()
{
    $('#username').html('Logged in as: ' + CMM.user);
    $('#userdiv').show();

    $('.T-account').show();

    CMM.getyears();
    CMM.getmonths();
    CMM.getallentries();
    CMM.getcategories();
}

CMM.dologout = function()
{
    window.location = '/logout';
}

// -----------------------------------------------------------------------------
// Main data display.
// -----------------------------------------------------------------------------

CMM.getyears = function()
{
    $.getJSON('/years', function(json) { CMM.gotyears(json); });
}

CMM.gotyears = function(json)
{
    var t = '- Select One -';
    var v = '';

    $('#years').empty();
    $('#years').append($('<option/>').attr('value', v).text(t));

    for (var item in json)
    {
        var opt = $('<option/>').attr('value', json[item]).text(json[item]);

        if (json[item] == CMM.year)
            $(opt).attr('selected', 'selected');

        $(opt).appendTo('#years');
    }

    if (CMM.year)
    {
        json.year = CMM.year;
        CMM.yearset(json);
    }
}

CMM.getmonths = function()
{
    $.getJSON('/months', function(json) { CMM.gotmonths(json); });
}

CMM.gotmonths = function(json)
{
    var t = '- Select One -';
    var v = '';

    $('#months').empty().append($('<option/>').attr('value', v).text(t));

    for (var item in json)
    {
        var attrs = { 'value' : item };

        if (json[item] == CMM.month)
            attrs['selected'] = 'selected';

        $('<option/>').attr(attrs).text(json[item]).appendTo('#months');
    }

    if (CMM.month)
    {
//      alert('here: ' + CMM.month);
        json.month = CMM.month;
        CMM.monthset(json);
    }
}

// Refresh content table.
CMM.getallentries = function()
{
    var params = { q_month: $('#months').val(), q_year: $('#years').val() };

    $.getJSON('/entry', function(json) { CMM.gotjsonentries(json); });
}

CMM.gotjsonentries = function(json)
{
    $('#contentpane').show();
    $('#entryform').show();
    $('#entrytable .Row').remove();

    $.each(json, function(k, v) { CMM.addjsonentry(k, v); });

    $('#entrytable tr:odd').addClass('Hilite1');

    CMM.getincome();
    CMM.getcapital();
    CMM.getcashflow();
    CMM.getbalance();

    CMM.updatetaccounts();
}

CMM.addjsonentry = function(idx, entry)
{
    // entry:
    // - date
    // - category
    // - categoryval
    // - description
    // - amount
    // - user
    // - idx

    var trID = 'tr_' + idx;
    var tr   = '#' + trID;

    if ($(tr).size() == 1) // Already exists, clear it.
        $(tr).empty();
    else if ($(tr).size() == 0) // Does not exist, create a new one.
        $('<tr/>').attr('id', trID).addClass('Row').appendTo('#entrytable');
    else
        alert('oops');

    // Build the row of data cells.
    $('<td/>').text(entry.idx).appendTo(tr);
    $('<td/>').text(entry.date).appendTo(tr);
    $('<td/>').text(entry.category).appendTo(tr);
    $('<td/>').text(entry.description).appendTo(tr);
    $('<td/>').text(entry.amount).addClass('Currency').appendTo(tr);

    var buttonID = 'button-ed_' + entry.idx;
    var button = '#' + buttonID;
    var attributes = { id: buttonID,
                       type: 'button',
                       value: 'Edit'
//                     disabled: 'disabled'
                     };

    $('<td/>').append($('<input/>').attr(attributes)).appendTo(tr);
    $(button).data('entry', entry);
    $(button).click(CMM.doedit);

    var buttonID = 'button-del_' + entry.idx;
    var button = '#' + buttonID;
    var attributes = { id: buttonID,
                       type: 'button',
                       value: 'Delete' };

    $('<td/>').append($('<input/>').attr(attributes)).appendTo(tr);
    $(button).data('entry', entry);
    $(button).click(CMM.dodelete);
/*
    var entry = { idx: entry.idx,
                  amount: entry.amount,
                  category: entry.categoryval, // NB!
                  description: entry.description,
                };
    $(button).data('entry', entry);
*/
}

// Get content of drop-down list.
CMM.getcategories = function()
{
    $.getJSON('/categories', function(json) { CMM.gotcategories(json); });
}

// Populate drop-down list.
CMM.gotcategories = function(json)
{
    var t = '- Select One -';
    var v = '';

    $('#categories').empty().append($('<option/>').attr('value', v).text(t));

    for (var item in json)
        $('<option/>').attr('value', item).text(json[item]).appendTo('#categories');
}

// Get content of drop-down list.
CMM.maybegetclients = function()
{
    var sel = $('#categories')[0];
    var cat = sel.options[sel.selectedIndex].value;

    if (cat == 'invoicebilled' ||
        cat == 'invoicepaid')
    {
        $('.tr_clients').show();
        $('#desc').attr('disabled', 'disabled');

        $.getJSON('/clients', function(json) { CMM.gotclients(json); });
    }
/*  else if (cat == 'ownerdraw' ||
             cat == 'capitalcontribution')
    {
        $('#desc').attr('disabled', 'disabled');
    }
*/  else
    {
        $('.tr_clients').hide();
        $('#desc').removeAttr('disabled');
    }
}

// Populate drop-down list.
CMM.gotclients = function(json)
{
    var t = '- Select One -';
    var v = '';

    $('#client').empty().append($('<option/>').attr('value', v).text(t));

    for (var item in json)
        $('<option/>').text(json[item]).appendTo('#client');
}

CMM.updatetaccounts = function()
{
    CMM.gettaccount('equity');
    CMM.gettaccount('asset');
    CMM.gettaccount('revenue');
    CMM.gettaccount('expense');
}

CMM.gettaccount = function(account)
{
    var tableid = account + 'table';

    $.getJSON('/' + account, function(json) { CMM.gottaccount(json, tableid); });
}

CMM.gottaccount = function(json, tableid)
{
    var table = '#' + tableid;
    $(table + ' .Row').remove();

    var debits  = 0;
    var credits = 0;

    // First, the entries themselves.
    $.each(json, function(k, v) {
        var trID = tableid + '_tr_' + k;
        var tr   = '#' + trID;

        $('<tr/>').attr('id', trID).addClass('Row').appendTo(table);

        // Build the row of data cells.
        $('<td/>').text(v.ref).appendTo(tr);
        $('<td/>').text(v.desc).appendTo(tr);
        if (v.category == 'debit')
        {
            $('<td/>').text(v.amount).addClass('Currency').appendTo(tr);
            $('<td/>').text('').appendTo(tr);
            
            debits += parseFloat(v.amount);
        }
        else
        {
            $('<td/>').text('').appendTo(tr);
            $('<td/>').text(v.amount).addClass('Currency').appendTo(tr);
            
            credits += parseFloat(v.amount);
        }
    });

    // Then, the totals.
    var trID = tableid + '_tr_totals';
    var tr   = '#' + trID;

    $('<tr/>').attr('id', trID).addClass('Row').appendTo(table);

    // Build the row of data cells.
    $('<th/>').appendTo(tr);
    $('<th/>').text('Totals').appendTo(tr);
    $('<td/>').text(debits).appendTo(tr);
    $('<td/>').text(credits).appendTo(tr);

    $(table + ' tr:odd').addClass('Hilite1');

    var assets_re = /^asset/i;
    var revenue_re = /^revenue/i;
    var expenses_re = /^expense/i;
    var equity_re = /^equity/i;

    if (tableid.search(assets_re) >= 0)
    {
        CMM.debits.assets = debits;
        CMM.credits.assets = credits;
    }
    else if (tableid.search(revenue_re) >= 0)
    {
        CMM.debits.revenue = debits;
        CMM.credits.revenue = credits;
    }
    else if (tableid.search(expenses_re) >= 0)
    {
        CMM.debits.expenses = debits;
        CMM.credits.expenses = credits;
    }
    else if (tableid.search(equity_re) >= 0)
    {
        CMM.debits.equity = debits;
        CMM.credits.equity = credits;
    }

    var totaldebits =
        CMM.debits.assets +
        CMM.debits.revenue +
        CMM.debits.expenses +
        CMM.debits.equity;

    var totalcredits =
        CMM.credits.assets +
        CMM.credits.revenue +
        CMM.credits.expenses +
        CMM.credits.equity;

    $('#totaldebits').text(totaldebits).addClass('Currency');
    $('#totalcredits').text(totalcredits).addClass('Currency');

    if (totaldebits !== totalcredits)
    {
//      alert('Debits and Credits are NOT equal!');
    }
}

CMM.setmonth = function() {
    var params = { month: $('#months').val() };

    $.post('/month', params, CMM.monthset, 'json');
}

CMM.monthset = function(json) {
    $('#span_month').text('Current month: ' + json.month);

    if (!$.browser.safari &&
        !$.browser.webkit) {
        $('#months option[selected=selected]').removeAttr('selected');

        if (typeof(json.month) == 'string') {
            $('#months option:contains("' + json.month + '")')
                .attr('selected', 'selected');
        } else {
            $('#months option[value=' + json.month + ']')
                .attr('selected', 'selected');
        }
    }

    CMM.getallentries();
}

CMM.setyear = function() {
    var params = { year: $('#years').val() };

    $.post('/year', params, CMM.yearset, 'json');
}

CMM.yearset = function(json) {
    // $('#span_month').text('Current month: ' + json.month);

    // if (!$.browser.safari &&
    //     !$.browser.webkit)
    // {
    //     $('#months option[selected=selected]').removeAttr('selected');

    //     if (typeof(json.month) == 'string')
    //     {
    //         $('#months option:contains("' + json.month + '")')
    //             .attr('selected', 'selected');
    //     }
    //     else
    //     {
    //         $('#months option[value=' + json.month + ']')
    //             .attr('selected', 'selected');
    //     }
    // }

    CMM.getallentries();
}

CMM.setupDays = function() {
    var t = '- Select One -';
    var v = '';

//  $('#days').empty().append($('<option/>').attr('value', v).text(t));
    $('#days').empty();

    for (var i = 1; i <= 31; i++) {
        $('<option/>').attr('value', i).text(i).appendTo('#days');
    }
}

// -----------------------------------------------------------------------------
// Add/Edit functionality.
// -----------------------------------------------------------------------------

// Add a new item for a logged in user.
CMM.addentry = function() {
    if (!CMM.user) {
        alert('You must be logged in to add entries');
        return;
    }

    var params = { category: $('#categories').val(),
                   desc: $('#desc').val(),
                   amount: $('#amount').val(),
                   client: $('#client').val(),
                   newclient: $('#new_client').val(),
                   day: $('#days').val()
    };

    if (params.category == '' ||
        params.amount == '' ||
        params.day == '') {
        alert('Please make sure you have category, amount, and day values.');
        return;
    }

    if (CMM.entry) {      // editing existing entry
//      alert('updating entry');
//      params.category = CMM.entry.category; // NB: override with keyword value
        params.category = CMM.entry.categoryval; // NB: override with keyword value
//      alert(params.category);
//      alert(CMM.entry.url);

        $.put('/entry/' + CMM.entry.idx, params, CMM.getallentries);

        CMM.entry = false;
    } else {            // adding new entry
//      alert(params.newclient);
        $.post('/entry', params, CMM.getallentries);
    }

    $('#desc').val('');
    $('#amount').val('');
//  $('#addbutton').val('Add');

    // FIXME: when Safari allows dynamic select change, swap enable this code.
    if (0) {
        $('#categories option[selected=selected]').removeAttr('selected');
        var sel = { selected: 'selected' };
        $('#categories option[value="0"]').attr(sel);
    } else {
        CMM.getcategories();
        $('.tr_clients').hide();
    }
}

CMM.dodelete = function() {
    var entry = $(this).data('entry');
    
    $.delete_('/entry/' + entry.idx, CMM.donedelete);
}

CMM.donedelete = function() {
    CMM.getallentries();
}

CMM.doedit = function() {
    var entry = $(this).data('entry');
    var sel = { 'selected' : 'selected' };

    $('#categories option[selected=selected]').removeAttr('selected');
    $('#categories option[value="' + entry.categoryval + '"]').attr(sel);

    $('#amount').val(entry.amount)
    $('#desc').val(entry.description)
//	$('#addbutton').val('Save');

	CMM.entry = entry;

    CMM.showdialog();

//	$.get(entry.url, function(xml) { CMM.gotoneentry(xml) });
}

// EXPERIMENTAL: instead of populating the form from the currently
// selected entry object, actually fetch the entry based on id.
CMM.gotoneentry = function(xml) {
//    alert($('entries', xml).text());
}

// -----------------------------------------------------------------------------
// Reports.
// -----------------------------------------------------------------------------
	
CMM.getincome = function() {
	$.getJSON('/income', CMM.gotincome);
}

CMM.gotincome = function(o) {
	$('#totalrevenue').text(o.revenue);
	$('#totalexpenses').text(o.expenses); // negative
	$('#netincome').text(o.netIncome);

	$('#income').show();
}

CMM.getcapital = function() {
	$.getJSON('/capital', CMM.gotcapital);
}

CMM.gotcapital = function(o) {
	$('#cap_begin').text(o.begin);
	$('#cap_contrib').text(o.contrib);
	$('#cap_income').text(o.netIncome);
	$('#cap_withdrawals').text(o.withdrawals); // negative
	$('#cap_change').text(o.change);
	$('#cap_end').text(o.end);

	$('#capital').show();
}

CMM.getcashflow = function() {
	$.getJSON('/cashflow', CMM.gotcashflow);
}

CMM.gotcashflow = function(o) {
	$('#cash_begin').text(o.begin);
	$('#cash_in').text(o.cashIn);
	$('#cash_out').text(o.cashOut);
	$('#cash_change').text(o.change);
	$('#cash_end').text(o.end);

	$('#cashflow').show();
}

CMM.getbalance = function() {
	$.get('/balance', CMM.gotbalance);
}

CMM.gotbalance = function(html) {
	$('#balance').empty().html(html).show();
}

CMM.hidelogin = function() {
	$('#loginform').hide('slow');
	$('#login_message').hide();
}

CMM.showdialog = function() {
	$('.tr_clients').hide();
	$('#dialog').dialog('open');
}

CMM.hidereports = function() {
    $('#allreports').toggle();
    $('#showreports').toggle();

    CMM.reports_visible = !CMM.reports_visible;
}

CMM.init = function() {
	$('#showlogin').click(function() { $('#loginform').slideToggle('slow'); });
	$('#cancelbutton').click(CMM.hidelogin);
	$('#loginbutton').click(CMM.dologin);

	$('#getentries').click(CMM.getallentries);
	$('#getincome').click(CMM.getincome);
	$('#getcapital').click(CMM.getcapital);
	$('#getcashflow').click(CMM.getcashflow);
	$('#getbalance').click(CMM.getbalance);

	$('#dotest').click(function() { CMM.gettaccount('expenses'); });
	$('#addentry').click(function() { CMM.showdialog(); });

//	$('#addbutton').click(CMM.addentry);

	$('#hidereports').click(function() { CMM.hidereports(); });
	$('#showreports').click(function() { CMM.hidereports(); });

	$('#months').change(CMM.setmonth);
	$('#years').change(CMM.setyear);
	$('#categories').change(CMM.maybegetclients);

	$('#dialog').dialog({
		modal: true,
		autoOpen: false,
		width: 500,
		buttons: {
			"Cancel": function() {
				$(this).dialog("close");
			},
			"Save": function() {
                CMM.addentry();
				$(this).dialog("close");
			}
		}
	});

	CMM.entry = false;

	CMM.debits = { assets: 0, revenue: 0, expenses: 0, equity: 0 };
	CMM.credits = { assets: 0, revenue: 0, expenses: 0, equity: 0 };

	CMM.setupDays();

	$.getJSON('/user', CMM.donelogin);
}
