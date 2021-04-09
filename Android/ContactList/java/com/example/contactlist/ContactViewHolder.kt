package com.example.contactlist

import android.view.View
import androidx.recyclerview.widget.RecyclerView
import kotlinx.android.synthetic.main.list_item.view.*

class ContactViewHolder(val root: View) : RecyclerView.ViewHolder(root) {
    val contactName = root.contact_name
    val contactPhoneNumber  = root.contact_phone_number
}
