-module(customer).
-export([runCustomer/0]).

runCustomer() ->
    receive
        {CustName, TargetAmount, OriginalAmount, CustBankList, true} ->
            timer:sleep(200),
            whereis(CustName) ! {CustName, TargetAmount, OriginalAmount, CustBankList},
            runCustomer();
        {CustName, TargetAmount, OriginalAmount, CustBankList} ->
            if
                TargetAmount == 0 ->
                    whereis(indTxn) ! {summaryData, borrowers, CustName, OriginalAmount, OriginalAmount},
					lists:foreach(fun(Bank) ->
					BankProcess = whereis(Bank),
					BankProcess ! {CustName, Bank}
					end, CustBankList);

                true ->
                    if
                        length(CustBankList) == 0 ->
                            AmountNotIssued = OriginalAmount - TargetAmount,
                            whereis(indTxn) ! {summaryData, borrowers, CustName, AmountNotIssued, OriginalAmount};
                        true ->
                            RandomBankIndex = rand:uniform(length(CustBankList)),
                            SelectedBank = lists:nth(RandomBankIndex, CustBankList),
                            timer:sleep(9 + rand:uniform(91)),
                            if
                                TargetAmount > 50 ->
                                    PartialLoanAmountRequest = rand:uniform(50);
                                true ->
                                    PartialLoanAmountRequest = TargetAmount
                            end,
                            whereis(indTxn) ! {customerIt, CustName, SelectedBank, PartialLoanAmountRequest},
                            whereis(SelectedBank) ! {CustName, PartialLoanAmountRequest, TargetAmount, OriginalAmount, SelectedBank, CustBankList},
                            runCustomer()
                    end
            end
    end.
