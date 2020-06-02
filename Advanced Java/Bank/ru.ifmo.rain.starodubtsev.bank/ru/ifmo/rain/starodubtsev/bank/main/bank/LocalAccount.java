package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.io.Serializable;
import java.rmi.RemoteException;

public class LocalAccount extends AbstractAccount implements Serializable {
	
	LocalAccount(final String id) {
		super(id);
	}
	
	LocalAccount(final RemoteAccount account) throws RemoteException {
		super(account.getId());
		amount.addAndGet(account.getAmount());
	}
}