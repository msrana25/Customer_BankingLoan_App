-module(money).
-export([start/1, indiviualTransactions/4, summaryListTraversal/5, summaryListTraversal_print_cust/1, summaryListTraversal_print_bank/1]).

start(Args) ->
  CustomerFile = lists:nth(1, Args),
  CustomerData = file:consult(CustomerFile),
  CustomerAmount = element(2, CustomerData),
  CustomerLen = length(CustomerAmount),
  BankFile = lists:nth(2, Args),
  BankData = file:consult(BankFile),
  BankAmount = element(2, BankData),
  BankLen = length(BankAmount),

  % Spawning Master Thread
  ArgLst = [],
  MainThread = spawn(money, indiviualTransactions, [ArgLst, self(),CustomerLen,BankLen]), % Pass the parent process id to indiviualTransactions 
  register(indTxn, MainThread),

  io:format("~n** The financial market is opening today **~n", []),
  io:format("~nStarting transaction log...~n", []),
  io:format("~n", []),

  BankMap = maps:from_list(BankAmount),
  CustBankList = maps:keys(BankMap),

  % Initialize Customer threads
  lists:foreach(fun(Customer) ->
    {CustName, CustAmount} = Customer,
    CustSpawnId = spawn(customer, runCustomer, []),
    register(CustName, CustSpawnId),
    InitializeFlag = true,
    whereis(CustName) ! {CustName, CustAmount, CustAmount, CustBankList, InitializeFlag}
  end, CustomerAmount),

  % Initialize Bank threads
  lists:foreach(fun(Bank) ->
    {BankName, _} = Bank,
    OriginalBankBalance = maps:get(BankName, BankMap),
    BankSpawnId = spawn(bank, runBank, [BankName, OriginalBankBalance, OriginalBankBalance,CustomerLen,[]]),
    register(BankName, BankSpawnId)
  end, BankAmount),

  receive
    done ->
      ok
  end.

indiviualTransactions(MergedList, ParentPid, CustomerLen, BankLen ) -> % Accept ParentPid as an argument
  receive
    {customerIt, CustomerName, BankName, RequestedAmount} ->
      io:fwrite("? ~p requests a loan of ~p dollar(s) from ~p bank~n", [CustomerName, RequestedAmount, BankName]),
      indiviualTransactions(MergedList, ParentPid, CustomerLen, BankLen );

    {bankIt, CustomName, LoanedAmount, BankName, IssuedFlag} ->
      if
        IssuedFlag == true -> 
          io:fwrite("$ ~p bank approves a loan of ~p dollars to ~p~n", [BankName, LoanedAmount, CustomName]);
        true -> 
          io:fwrite("$ ~p bank denies a loan of ~p dollars to ~p~n", [BankName, LoanedAmount, CustomName])
      end,
      indiviualTransactions(MergedList, ParentPid, CustomerLen, BankLen );

    {summaryData, borrowers, CustName, AcquiredAmt, OriginalAmount} ->
      SummaryString = [cust, CustName, AcquiredAmt, OriginalAmount],
      NewMergedList = lists:append(MergedList, [SummaryString]),
	  if
        length(NewMergedList) =:= CustomerLen + BankLen ->
          io:format("~n", []),
          io:format("** Banking Report ** ~n"),
          Mll = length(MergedList),
          ListCust = [],
          ListBank = [],
          summaryListTraversal(Mll, NewMergedList, ListCust, ListBank, ParentPid),
          ok;
        true ->
          ok
      end,
      indiviualTransactions(NewMergedList, ParentPid, CustomerLen, BankLen );

    {summaryData, lenders, BankName, OriginalStartingAmount,BalanceAmount} ->
      BankSummary = [fi, BankName, BalanceAmount, OriginalStartingAmount],
      IsMember = lists:member(BankSummary, MergedList),
      NewMergedList = case IsMember of
        true -> MergedList;
        false -> lists:append(MergedList, [BankSummary])
      end,
	  if
        length(NewMergedList) =:= CustomerLen + BankLen ->
          io:format("~n", []),
          io:format("** Banking Report ** ~n"),
          Mll = length(MergedList),
          ListCust = [],
          ListBank = [],
          summaryListTraversal(Mll, NewMergedList, ListCust, ListBank, ParentPid),
          ok;
        true ->
          ok
      end,
      indiviualTransactions(NewMergedList, ParentPid, CustomerLen, BankLen )
  end.


summaryListTraversal(0, _, ListCust, ListBank, Pid) ->
  io:format("~n", []),
  io:format("Customers: ~n"),
  {SumObjective, SumReceived} = summaryListTraversal_print_cust(ListCust),
  io:format("----~n"),
  io:format("Total: objective ~w, received ~w~n", [SumObjective,SumReceived]),
  io:format("~n", []),
  io:format("Banks: ~n"),
  {SumOriginal, SumBalance} = summaryListTraversal_print_bank(ListBank),
  io:format("----~n"),
  io:format("Total: original ~w, loaned ~w~n", [SumOriginal, SumOriginal-SumBalance]),
  io:format("~n", []),
  io:format("The financial market is closing for the day... ~n", []),
  io:format("~n", []),
  Pid ! done;

summaryListTraversal(Mll, MergedList, ListCust, ListBank, Pid) ->
  RandomIndex = rand:uniform(Mll),
  SelectedTuple = lists:nth(RandomIndex, MergedList),
  Id = lists:nth(1, SelectedTuple),
  if
    Id == cust ->
      CustName = lists:nth(2, SelectedTuple),
      AcquiredAmt = lists:nth(3, SelectedTuple),
      OrigAskAmount = lists:nth(4, SelectedTuple),
      NewListCust = lists:append(ListCust, [CustName, OrigAskAmount, AcquiredAmt]),
      New_list = lists:delete(SelectedTuple, MergedList),
      New_length = length(New_list),
      summaryListTraversal(New_length, New_list, NewListCust, ListBank, Pid);
    true ->
      BankName = lists:nth(2, SelectedTuple),
      BalanceAmount = lists:nth(3, SelectedTuple),
      OriginalMoney = lists:nth(4, SelectedTuple),
      NewListBank = lists:append(ListBank, [BankName, BalanceAmount, OriginalMoney]),
      New_list = lists:delete(SelectedTuple, MergedList),
      New_length = length(New_list),
      summaryListTraversal(New_length, New_list, ListCust, NewListBank, Pid)
  end.

summaryListTraversal_print_cust([]) ->
  {0, 0};

summaryListTraversal_print_cust([Name, Objective, Received | Rest]) ->
  io:format("~s: objective ~w, received ~w~n", [Name, Objective, Received]),
  {SumObjective, SumReceived} = summaryListTraversal_print_cust(Rest),
  {SumObjective + Objective, SumReceived + Received}.

summaryListTraversal_print_bank([]) ->
  {0, 0};

summaryListTraversal_print_bank([Name, Balance, Original | Rest]) ->
  io:format("~s: original ~w, balance ~w~n", [Name, Original, Balance]),
  {SumOriginal, SumBalance} = summaryListTraversal_print_bank(Rest),
  {SumOriginal + Original, SumBalance + Balance}.
