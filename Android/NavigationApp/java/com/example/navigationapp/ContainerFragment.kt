package com.example.navigationapp

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.fragment.app.Fragment
import kotlinx.android.synthetic.main.fragment_container.*

class ContainerFragment : Fragment() {

    private var count = 0

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        if (savedInstanceState != null) {
            count = childFragmentManager.backStackEntryCount
        }
        return inflater.inflate(R.layout.fragment_container, container, false)
    }

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)
        arguments?.let {
            activity?.title = it.getString(MainActivity.NAME)
            button_open_child_fragment.setOnClickListener {
                val childKey = getFragmentString()
                val fragment = ChildFragment()
                val bundle = Bundle()
                bundle.putString(KEY, childKey)
                fragment.arguments = bundle
                childFragmentManager.beginTransaction()
                    .replace(R.id.container_fragment, fragment, childKey)
                    .addToBackStack(childKey)
                    .commit()
            }
        }

        childFragmentManager.addOnBackStackChangedListener {
            count = childFragmentManager.backStackEntryCount
        }
    }

    private fun getFragmentString(): String {
        var res = "0"
        for (i in 1..count) {
            res += "->$i"
        }
        return res
    }

    companion object {
        const val KEY = "fragmentKey"
    }

}