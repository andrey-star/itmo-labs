package ru.ifmo.rain.starodubtsev.bank.main;

import ru.ifmo.rain.starodubtsev.bank.main.bank.Bank;
import ru.ifmo.rain.starodubtsev.bank.main.bank.RemoteBank;

import java.net.MalformedURLException;
import java.rmi.ConnectException;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

public class Server {
	
	private final static int PORT = 8888;
	private static final int REGISTRY_PORT = 1099;
	private static Registry registry;
	
	public static void main(String[] args) {
		try {
			createRegistry(REGISTRY_PORT);
			Bank bank = new RemoteBank(PORT);
			try {
				UnicastRemoteObject.exportObject(bank, PORT);
				Naming.rebind("//localhost/bank", bank);
			} catch (final RemoteException e) {
				System.out.println("Cannot export object: " + e.getMessage());
				e.printStackTrace();
			}
		} catch (final RemoteException e) {
			System.out.println("Unable to create RMI registry: " + e.getMessage());
			e.printStackTrace();
		} catch (final MalformedURLException e) {
			System.out.println("Malformed URL");
		}
		System.out.println("Server started");
	}
	
	public static void createRegistry(int port) throws RemoteException {
		try {
			registry = LocateRegistry.getRegistry(port);
			registry.list();
		} catch (ConnectException e) {
			registry = LocateRegistry.createRegistry(port);
		}
	}
	
}
