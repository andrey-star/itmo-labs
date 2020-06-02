package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.io.Serializable;
import java.util.concurrent.atomic.AtomicLong;

public class AbstractAccount implements Serializable {
	
	protected final String id;
	protected final AtomicLong amount;
	
	AbstractAccount(final String id) {
		this.id = id;
		amount = new AtomicLong();
	}
	
	public String getId() {
		return id;
	}
	
	public long getAmount() {
		Logger.info("Getting amount of money for remote account " + id);
		return amount.longValue();
	}
	
	public void addFunds(long amount) {
		Logger.info("Adding money to local account " + id);
		this.amount.addAndGet(amount);
	}
	
}
