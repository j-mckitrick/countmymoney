// Count My Money - requires jQuery
// TO DO: toggle reports
// TO DO: trap RETURN key like mood genie js

function CMM() {}

// -----------------------------------------------------------------------------
// Authentication and login.
// -----------------------------------------------------------------------------

// Send AJAX login form.
CMM.dologin = function()
{
    var params = { user: $("#user").val(),
				   pass: $("#pass").val(),
				   persist: $("#persist").val() };

    $.post("/login", params, function(json) { CMM.donelogin(json) }, "json");
}

// Handle results of login attempt or successful signup.
CMM.donelogin = function(json)
{
	if (json.user)
	{
//		alert(json.user);

		CMM.user = json.user;

		if (json.month)
		{
//	    alert(json.month);
			CMM.month = json.month;
		}

		if (json.year) {
//			alert(json.year);
			CMM.year = json.year;
		}

		$("#showlogin").text("Logout");
		$("#showlogin").unbind('click', CMM.togglelogin).click(CMM.dologout);

		$("#loginform").hide("slow", CMM.doneloginOK);
	}
	else if (json.fail == "username")
	{
		$('#login_message').addClass('Red').text('Missing username');
	}
	else if (json.fail == "password")
	{
		$('#login_message').addClass('Red').text('Missing password');
	}
	else if (json.fail == "baduser")
	{
		$('#login_message').addClass('Red').text('Bad username');
	}
	else if (json.fail == "badpass")
	{
		$('#login_message').addClass('Red').text('Bad password');
	}
}

// Update UI for a successful login.
CMM.doneloginOK = function()
{
	$("#username").html('Logged in as: ' + CMM.user);
	$("#userdiv").show();

	$('.T-account').show();

//	CMM.getyears();
	CMM.getmonths();
	CMM.getallentries();
	CMM.getcategories();

	// This should already be called in gotallentries().
//    CMM.updatetaccounts();
}

CMM.dologout = function()
{
	window.location = "/logout";
}

// -----------------------------------------------------------------------------
// Main data display.
// -----------------------------------------------------------------------------

CMM.getyears = function()
{
	$.getJSON("/years", function(json) { CMM.gotyears(json) });
}

CMM.gotyears = function(json)
{
	var t = '- Select One -';
	var v = '';

	$('#years').empty().append($('<option/>').attr('value', v).text(t));

	for (var item in json)
		$('<option/>').attr('value', item).text(json[item]).appendTo('#years');

	if (CMM.year)
	{
//	alert('here: ' + CMM.year);
		json.year = CMM.year;
		CMM.yearset(json);
	}
}

CMM.getmonths = function()
{
	$.getJSON("/months", function(json) { CMM.gotmonths(json); });
}

CMM.gotmonths = function(json)
{
	var t = '- Select One -';
	var v = '';

	$('#months').empty().append($('<option/>').attr('value', v).text(t));

	for (var item in json)
		$('<option/>').attr('value', item).text(json[item]).appendTo('#months');

	if (CMM.month)
	{
		alert('here: ' + CMM.month);
		json.month = CMM.month;
		CMM.monthset(json);
	}
}

// Refresh content table.
CMM.getallentries = function()
{
	var params = { q_month: $("#months").val(), q_year: $('#years').val() };

	$.get("/entry", function(xml) { CMM.gotallentries(xml) });
//    $.get("/entry", params, function(xml) { CMM.gotallentries(xml) });
}

// Clear content table and show new data.
CMM.gotallentries = function(xml)
{
//    alert('here ' + $('entries', xml).text());
	$("#contentpane").show();
	$("#entryform").show();
	$('#entrytable .Row').remove();

	CMM.gotxmlentries(xml);
}

	// Table update from XML - core of the application.
CMM.gotxmlentries = function(xml)
{
//    alert('here ' + $('entries', xml).text());
	$('entry', xml).each(function() {
		var entryurl = $(this).attr('url');
		var entryidx = $(this).attr('idx');
		var trID = 'tr_' + entryidx;
		var tr   = '#' + trID;

		if ($(tr).size() == 1) // Already exists, clear it.
		{
			$(tr).empty();
		}
		else if ($(tr).size() == 0) // Does not exist, create a new one.
		{
			$('<tr/>').attr('id', trID).addClass('Row').appendTo('#entrytable');
		}
		else
		{
			alert('oops');
		}

		// Build the row of data cells.
		$('<td/>').text($(this).attr('date')).appendTo(tr);
		$('<td/>').text($(this).attr('category')).appendTo(tr);
		$('<td/>').text($(this).attr('description')).appendTo(tr);
		$('<td/>').text($(this).text()).appendTo(tr);

		var buttonID = 'button-del_' + entryidx;
		var button = '#' + buttonID;
		var attributes = { id: buttonID,
						   type: "button",
						   value: "Delete" };

		$('<td/>').append($('<input/>').attr(attributes)).appendTo(tr);
		$(button).data('url', entryurl);
		$(button).click(CMM.dodelete);

		var buttonID = 'button-ed_' + entryidx;
		var button = '#' + buttonID;
		var attributes = { id: buttonID,
						   type: "button",
						   value: "Edit",
//						   disabled: "disabled"
						 };

		$('<td/>').append($('<input/>').attr(attributes)).appendTo(tr);
		$(button).data('url', entryurl);
		$(button).click(CMM.doedit);

		var entry = { url: entryurl,
					  amount: $(this).text(),
					  category: $(this).attr('categoryvalue'), // NB!
					  description: $(this).attr('description'),
					};
		$(button).data('entry', entry);
	});

	$('#entrytable tr:odd').addClass('Hilite1');

	CMM.updatetaccounts();

	CMM.getincome();
	CMM.getcapital();
	CMM.getcashflow();
	CMM.getbalance();
}

// Get content of drop-down list.
CMM.getcategories = function()
{
	$.getJSON("/categories", function(json) { CMM.gotcategories(json); });
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
//    if ($('#categories option:selected'))
	var sel = $('#categories')[0];
	var cat = sel.options[sel.selectedIndex].value;
    
	if (cat == 'invoicebilled' ||
		cat == 'invoicepaid')
	{
		$('.tr_clients').show();
		$('#desc').attr('disabled', 'disabled');
//	$('#desc').val('CLIENT');
//	$('#desc_label').text('Add_New_Client');

		$.getJSON("/clients", function(json) { CMM.gotclients(json) });
	}
	else
	{
		$('.tr_clients').hide();
		$('#desc').removeAttr('disabled');
//	$('#desc').val('').removeAttr('disabled');
//	$('#desc_label').text('Description');
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
	CMM.gettaccount('draw');
	CMM.gettaccount('assets');
	CMM.gettaccount('revenue');
	CMM.gettaccount('expenses');
}

CMM.gettaccount = function(account)
{
	var tableid = account + 'table';

	$.get("/" + account, function(xml) { CMM.gottaccount(xml, tableid) });
}

CMM.gottaccount = function(xml, tableid)
{
//    alert($('row', xml).text());

	var table = '#' + tableid;
	$(table + ' .Row').remove();

	var debits = 0;
	var credits = 0;
	var i = 1;
	$('row', xml).each(function() {
		var trID = tableid + '_tr_' + i++;
		var tr   = '#' + trID;

		$('<tr/>').attr('id', trID).addClass('Row').appendTo(table);

		// Build the row of data cells.
		$('<td/>').text($(this).attr('desc')).appendTo(tr);
		if ($(this).attr('category') == 'DEBIT')
		{
			$('<td/>').text($(this).text()).appendTo(tr);
			$('<td/>').text('').appendTo(tr);
			
			debits += parseFloat($(this).text());
		}
		else
		{
			$('<td/>').text('').appendTo(tr);
			$('<td/>').text($(this).text()).appendTo(tr);

			credits += parseFloat($(this).text());
		}
	});

	var trID = tableid + '_tr_totals';
	var tr   = '#' + trID;

	$('<tr/>').attr('id', trID).addClass('Row').appendTo(table);

	// Build the row of data cells.
	$('<th/>').text('Totals').appendTo(tr);
	$('<td/>').text(debits).appendTo(tr);
	$('<td/>').text(credits).appendTo(tr);

	$(table + ' tr:odd').addClass('Hilite1');

	var assets_re = /^assets/i;
	var revenue_re = /^revenue/i;
	var expenses_re = /^expenses/i;
	var draw_re = /^draw/i;

//    alert('tableid: ' + tableid);
	if (tableid.search(assets_re) >= 0)
	{
//	alert('Search: ' + assets_re + ' ' + tableid.search(assets_re));
		CMM.debits.assets = debits;
		CMM.credits.assets = credits;
//	alert('Assets: ' + CMM.debits.assets + ':' + CMM.credits.assets);
	}
	else if (tableid.search(revenue_re) >= 0)
	{
//	alert('Search: ' + revenue_re + ' ' + tableid.search(revenue_re));
		CMM.debits.revenue = debits;
		CMM.credits.revenue = credits;
//	alert('Revenue: ' + CMM.debits.revenue + ':' + CMM.credits.revenue);
	}
	else if (tableid.search(expenses_re) >= 0)
	{
//	alert('Search: ' + expenses_re + ' ' + tableid.search(expenses_re));
		CMM.debits.expenses = debits;
		CMM.credits.expenses = credits;
//	alert('Expense: ' + CMM.debits.expenses + ':' + CMM.credits.expenses);
	}
	else if (tableid.search(draw_re) >= 0)
	{
//	alert('Search: ' + draw_re + ' ' + tableid.search(draw_re));
		CMM.debits.draw = debits;
		CMM.credits.draw = credits;
//	alert('Draw: ' + CMM.debits.draw + ':' + CMM.credits.draw);
	}

	var totaldebits = CMM.debits.assets + CMM.debits.revenue +
	CMM.debits.expenses + CMM.debits.draw;
	var totalcredits = CMM.credits.assets + CMM.credits.revenue +
	CMM.credits.expenses + CMM.credits.draw;

//    alert('Debits = ' + totaldebits);
//    alert('Credits = ' + totalcredits);

	$('#totaldebits').text(totaldebits);
	$('#totalcredits').text(totalcredits);

	if (totaldebits == totalcredits)
	{
//	alert('Debits and Credits are equal!');
	}
}

CMM.setmonth = function()
{
//    var params = { month: $("#months").val() };
	var params = { };

	// NB: for some reason, the version without 'params' does not
	// receive the callback after the POST.  It seems that even though
	// 'params' is supposed to be optional, if it is missing, the content type
	// 'json' is not sent, so the callback expects an xml response.
	$.post("/month/" + $("#months").val(), params, CMM.monthset, "json");
//    $.post("/month/" + $("#months").val(), CMM.monthset, "json");
}

CMM.monthset = function(json)
{
//    alert('set ' + json);
//    alert('set ' + json.month);
	$('#span_month').text(json.month);

	$('#months option[selected=selected]').removeAttr("selected");

//    alert(typeof(json.month));

	if (typeof(json.month) == 'string')
	{
		// Month is a string
		$('#months option:contains("' + json.month + '")').attr("selected", "selected");
	}
	else
	{
		// Month is an integer
//	$('#months option[value=' + json.month + ']').attr("selected", "selected");
	}

	CMM.getallentries();
}

CMM.setupDays = function()
{
	var t = '- Select One -';
	var v = '';

	$('#days').empty().append($('<option/>').attr('value', v).text(t));

	for (var i = 1; i <= 31; i++)
		$('<option/>').attr('value', i).text(i).appendTo('#days');
}

// -----------------------------------------------------------------------------
// Add/Edit functionality.
// -----------------------------------------------------------------------------

// Add a new item for a logged in user.
CMM.addentry = function()
{
	if (!CMM.user)
	{
		alert('You must be logged in to add entries');
		return;
	}

	var params = { category: $("#categories").val(),
				   desc: $("#desc").val(),
				   amount: $("#amount").val(),
				   client: $('#client').val(),
				   newclient: $('#new_client').val(),
				   day: $('#days').val()
	};

	if (params.category == '' ||
		params.amount == '' ||
		params.day == '')
	{
		alert('Please make sure you have category, amount, and day values.');
		return;
	}

	if (CMM.entry)		// editing existing entry
	{
		params.category = CMM.entry.category; // NB: override with keyword value
//	    alert(params.category);
//	    alert(CMM.entry.url);
		$.post(CMM.entry.url, params, CMM.gotxmlentries);

		CMM.entry = false;
	}
	else			// adding new entry
	{
//	    alert(params.newclient);
		$.post("/entry", params, CMM.gotxmlentries);
	}

	$("#desc").val('');
	$("#amount").val('');
	$('#addbutton').val('Add');

	// FIXME: when Safari allows dynamic select change, swap enable this code.
	if (0)
	{
		$('#categories option[selected=selected]').removeAttr("selected");
		var sel = { selected: "selected" };
		$('#categories option[value="0"]').attr(sel);
	}
	else
	{
		CMM.getcategories();
		$('.tr_clients').hide();
	}
}

CMM.dodelete = function()
{
	var url = $(this).data('url');
	
	$.delete_(url, CMM.donedelete);
}

CMM.donedelete = function()
{
	alert('delete complete');
	CMM.getallentries();
}

CMM.doedit = function()
{
	var entry = $(this).data('entry');

//    alert(entry.category);
	var sel = { selected: "selected" };
	$('#categories option[value="' + entry.category + '"]').attr(sel);

	$('#amount').val(entry.amount)
	$('#desc').val(entry.description)
	$('#addbutton').val('Save');

	CMM.entry = entry;

	$.get(entry.url, function(xml) { CMM.gotoneentry(xml) });
}

// EXPERIMENTAL: instead of populating the form from the currently
// selected entry object, actually fetch the entry based on id.
CMM.gotoneentry = function(xml)
{
//    alert($('entries', xml).text());
}

// -----------------------------------------------------------------------------
// Reports.
// -----------------------------------------------------------------------------
	
CMM.getincome = function()
{
	$.get("/income", CMM.gotincome);
}

CMM.gotincome = function(xml)
{
	var rev = $(xml).find('revenue').text();
	var exp = $(xml).find('expenses').text();
	var net = $(xml).find('net-income').text();
/*
  $('<tr/>').attr('id', 'revenuerow').appendTo('#incomestatement');
  $('<tr/>').attr('id', 'expensesrow').appendTo('#incomestatement');
  $('<tr/>').attr('id', 'totalrow').appendTo('#incomestatement');

  $('<td/>').text('').appendTo('#revenuerow');
  $('<td/>').text('').appendTo('#revenuerow');
  $('<td/>').text(rev).appendTo('#revenuerow');
*/
	$('#totalrevenue').text(rev);
	$('#totalexpenses').text(exp);
	$('#netincome').text(net);

	$('#income').show();
}

CMM.getbalance = function()
{
	$.get("/balance", CMM.gotbalance);
}

CMM.gotbalance = function(html)
{
	$('#balance').empty().html(html).show();
}

CMM.getcapital = function()
{
	$.get("/capital", CMM.gotcapital);
}

CMM.gotcapital = function(xml)
{
	var begin = $(xml).find('begin').text();
	var contrib = $(xml).find('contrib').text();
	var income = $(xml).find('net-income').text();
	var withdrawals = $(xml).find('withdrawals').text();
	var change = $(xml).find('change').text();
	var end = $(xml).find('end').text();

	$('#cap_begin').text(begin);
	$('#cap_contrib').text(contrib);
	$('#cap_income').text(income);
	$('#cap_withdrawals').text(withdrawals);
	$('#cap_change').text(change);
	$('#cap_end').text(end);

	$('#capital').show();
}

CMM.getcashflow = function()
{
	$.get("/cashflow", CMM.gotcashflow);
}

CMM.gotcashflow = function(xml)
{
	var begin = $(xml).find('begin').text();
	var cash_in = $(xml).find('cash-in').text();
	var cash_out = $(xml).find('cash-out').text();
	var change = $(xml).find('change').text();
	var end = $(xml).find('end').text();

	$('#cash_begin').text(begin);
	$('#cash_in').text(cash_in);
	$('#cash_out').text(cash_out);
	$('#cash_change').text(change);
	$('#cash_end').text(end);

	$('#cashflow').show();
}

CMM.hidelogin = function()
{
	$("#loginform").hide("slow");
	$('#login_message').hide();
}

CMM.init = function()
{
	$("#showlogin").click(function() { $("#loginform").slideToggle("slow") });
//    $("#showlogin").click(function() { $("#loginform").toggle("slow") });
	$("#cancelbutton").click(CMM.hidelogin);
	$("#loginbutton").click(CMM.dologin);

	$("#getentries").click(CMM.getallentries);
	$("#getincome").click(CMM.getincome);
	$("#getcapital").click(CMM.getcapital);
	$("#getcashflow").click(CMM.getcashflow);
	$("#getbalance").click(CMM.getbalance);

	$("#dotest").click(function() { CMM.gettaccount('expenses') });

	$("#addbutton").click(CMM.addentry);

	$('#months').change(CMM.setmonth);
	$('#categories').change(CMM.maybegetclients);

	CMM.entry = false;

	CMM.debits = { assets:0, revenue:0, expenses:0, draw:0 };
	CMM.credits = { assets:0, revenue:0, expenses:0, draw:0 };

	CMM.setupDays();

	$.getJSON("/user", CMM.donelogin);
}

