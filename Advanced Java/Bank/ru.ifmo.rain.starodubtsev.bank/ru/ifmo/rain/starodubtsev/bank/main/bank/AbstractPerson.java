package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.io.Serializable;

public class AbstractPerson implements Serializable {
	
	protected final String firstName;
	protected final String lastName;
	protected final String passportId;
	
	public AbstractPerson(String firstName, String lastName, String passportId) {
		this.firstName = firstName;
		this.lastName = lastName;
		this.passportId = passportId;
	}
	
	String getAccountId(final String subId) {
		return getPassportId() + ":" + subId;
	}
	
	public String getFirstName() {
		return firstName;
	}
	
	public String getLastName() {
		return lastName;
	}
	
	public String getPassportId() {
		return passportId;
	}
}
	

