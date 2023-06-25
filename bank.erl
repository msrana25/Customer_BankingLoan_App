-module(bank).
-export([runBank/5]).

runBank(BankName, OriAmount, BankBalance, CustomerLen, CustomerList) ->
    receive
        {CustName, Bank} ->
            NewCustomerList = lists:append(CustomerList, [CustName]),
            if 
                length(NewCustomerList) == CustomerLen ->
                    whereis(indTxn) ! {summaryData, lenders, Bank, OriAmount, BankBalance};
                true ->
                    ok
            end,
            runBank(Bank, OriAmount, BankBalance, CustomerLen, NewCustomerList);

        {CustName, PartialLoanAmountRequest, TargetAmount, OriginalAmount, SelectedBank, CustBankList} ->
            if
                BankBalance >= PartialLoanAmountRequest ->
                    UpdatedCustomerPendingLoanAmount = TargetAmount - PartialLoanAmountRequest,
                    UpdatedBankBalance = BankBalance - PartialLoanAmountRequest,
                    whereis(indTxn) ! {bankIt, CustName, PartialLoanAmountRequest, SelectedBank, true},
                    whereis(CustName) ! {CustName, UpdatedCustomerPendingLoanAmount, OriginalAmount, CustBankList};
            
                true ->
                    UpdatedBankBalance = BankBalance,
                    NewCustBankList = lists:delete(SelectedBank, CustBankList),
                    NewCustomerList = lists:append(CustomerList, [CustName]),
                    whereis(indTxn) ! {bankIt, CustName, PartialLoanAmountRequest, SelectedBank, false},
                    whereis(CustName) ! {CustName, TargetAmount, OriginalAmount, NewCustBankList},
                    if
                        length(NewCustomerList) == CustomerLen ->
                            whereis(indTxn) ! {summaryData, lenders, BankName, OriAmount, BankBalance};
                        true ->
                            ok
                    end,
                    runBank(BankName, OriAmount, UpdatedBankBalance, CustomerLen, NewCustomerList)
            end,
            runBank(BankName, OriAmount, UpdatedBankBalance, CustomerLen, CustomerList)
    end.
