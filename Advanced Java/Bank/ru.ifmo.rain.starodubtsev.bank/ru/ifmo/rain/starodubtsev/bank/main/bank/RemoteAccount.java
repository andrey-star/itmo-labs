package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RemoteAccount extends Remote {
	
	String getId() throws RemoteException;
	
	long getAmount() throws RemoteException;
	
	void addFunds(long amount) throws RemoteException;
}