package com.example.contactlist

import android.Manifest
import android.content.pm.PackageManager
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import android.widget.Toast
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import android.content.Intent
import android.content.res.Resources
import android.net.Uri


class MainActivity : AppCompatActivity() {
    companion object {
        private const val REQUEST_CODE = 4
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        if (ContextCompat.checkSelfPermission(
                this@MainActivity,
                Manifest.permission.READ_CONTACTS
            )
            != PackageManager.PERMISSION_GRANTED
        ) {
            ActivityCompat.requestPermissions(
                this@MainActivity,
                arrayOf(Manifest.permission.READ_CONTACTS),
                REQUEST_CODE
            )
        } else {
            displayContacts()
        }

    }

    private fun displayContacts() {
        val viewManager = LinearLayoutManager(this)
        val myAdapter = ContactAdapter(fetchAllContacts()) {
            val intent = Intent(Intent.ACTION_DIAL)
            intent.data = Uri.parse("tel:${it.phoneNumber}")
            startActivity(intent)
        }
        findViewById<RecyclerView>(R.id.my_recycler_view).apply {
            layoutManager = viewManager
            adapter = myAdapter
        }
        Toast.makeText(
            this,
            resources.getQuantityString(
                R.plurals.contacts_toast,
                myAdapter.itemCount,
                myAdapter.itemCount
            ),
            Toast.LENGTH_LONG
        )
            .show()
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<String>,
        grantResults: IntArray
    ) {
        when (requestCode) {
            REQUEST_CODE -> {
                if (grantResults.isNotEmpty() && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    displayContacts()
                } else {
                    Toast.makeText(this, "Read Contacts permission denied", Toast.LENGTH_LONG)
                        .show()
                }
                return
            }
        }
    }

}
