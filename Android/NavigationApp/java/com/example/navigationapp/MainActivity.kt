package com.example.navigationapp

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.SparseArray
import android.view.MenuItem
import androidx.fragment.app.Fragment
import com.google.android.material.bottomnavigation.BottomNavigationView
import com.google.android.material.navigation.NavigationView
import kotlinx.android.synthetic.main.activity_main.*

class MainActivity : AppCompatActivity() {

    private var savedStateSparseArray = SparseArray<Fragment.SavedState>()
    private var currentSelectItemId = R.id.nav_home
    private val tabIdToName = mapOf(
        R.id.nav_home to "Home",
        R.id.nav_favorites to "Favorites",
        R.id.nav_search to "Search"
    )
    private val nameToTabId = mapOf(
        "Home" to R.id.nav_home,
        "Favorites" to R.id.nav_favorites,
        "Search" to R.id.nav_search
    )
    private val tabIds = listOf(R.id.nav_home, R.id.nav_favorites, R.id.nav_search)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        if (savedInstanceState != null) {
            savedStateSparseArray =
                savedInstanceState.getSparseParcelableArray(CONTAINER)
                    ?: SparseArray()
            currentSelectItemId = savedInstanceState.getInt(CURRENT_TAB)
        }
        setContentView(R.layout.activity_main)

        supportActionBar?.setDisplayShowHomeEnabled(true)
        supportActionBar?.setDisplayHomeAsUpEnabled(true)

        if (bottom_navigation != null) {
            bottom_navigation.setOnNavigationItemSelectedListener(bottomNavListener)
        } else {
            side_navigation.setNavigationItemSelectedListener(sideNavListener)
        }
        val fragmentName = tabIdToName[currentSelectItemId]
        fragmentName?.let { setFragment(fragmentName, currentSelectItemId) }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.putSparseParcelableArray(CONTAINER, savedStateSparseArray)
        outState.putInt(CURRENT_TAB, currentSelectItemId)
    }

    private val bottomNavListener = BottomNavigationView.OnNavigationItemSelectedListener {
        navigationItemSelectedListener(it)
    }

    private val sideNavListener = NavigationView.OnNavigationItemSelectedListener {
        navigationItemSelectedListener(it)
    }

    private fun navigationItemSelectedListener(item: MenuItem): Boolean {
        val name = tabIdToName[item.itemId]
        if (name != null) {
            setFragment(name, item.itemId)
            return true
        }
        return false
    }

    private fun setFragment(name: String, itemId: Int) {
        if (supportFragmentManager.findFragmentByTag(name) == null) {
            saveFragmentState()
            createFragment(name, itemId)
            currentSelectItemId = itemId
        }
    }

    private fun saveFragmentState() {
        val currentFragment = supportFragmentManager.findFragmentById(R.id.container_fragment)
        if (currentFragment != null) {
            savedStateSparseArray.put(
                currentSelectItemId,
                supportFragmentManager.saveFragmentInstanceState(currentFragment)
            )
        }
    }

    private fun createFragment(name: String, itemId: Int) {
        val fragment = ContainerFragment()
        val bundle = Bundle()
        bundle.putString(NAME, name)
        fragment.arguments = bundle
        fragment.setInitialSavedState(savedStateSparseArray[itemId])
        val inAnim: Int
        val outAnim: Int
        if (bottom_navigation != null) {
            if (tabIds.indexOf(currentSelectItemId) < tabIds.indexOf(itemId)) {
                inAnim = R.anim.from_left_in
                outAnim = R.anim.from_left_out
            } else {
                inAnim = R.anim.from_right_in
                outAnim = R.anim.from_right_out
            }
        } else {
            if (tabIds.indexOf(currentSelectItemId) < tabIds.indexOf(itemId)) {
                inAnim = R.anim.from_down_in
                outAnim = R.anim.from_down_out
            } else {
                inAnim = R.anim.from_up_in
                outAnim = R.anim.from_up_out
            }
        }
        supportFragmentManager.beginTransaction().setCustomAnimations(inAnim, outAnim)
            .replace(R.id.container_fragment, fragment, name)
            .commit()
    }

    override fun onBackPressed() {
        supportFragmentManager.fragments.forEach { fragment ->
            if (fragment != null && fragment.isVisible) {
                if (fragment.childFragmentManager.backStackEntryCount > 0) {
                    fragment.childFragmentManager.popBackStack()
                    return
                } else if (fragment.arguments != null) {
                    val name = fragment.arguments?.get(NAME) as String
                    val id = nameToTabId[name]
                    val index = tabIds.indexOf(id)
                    if (index > 0) {
                        val prevId = tabIds[tabIds.indexOf(id) - 1]
                        if (bottom_navigation != null) {
                            bottom_navigation.selectedItemId = prevId
                        }
                        val prevName = tabIdToName[prevId]
                        prevName?.let { setFragment(prevName, prevId) }
                        return
                    }
                }
            }
            super.onBackPressed()
        }
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        val id = item.itemId
        if (id == android.R.id.home) {
            onBackPressed()
        }
        return super.onOptionsItemSelected(item)
    }

    companion object {
        const val CONTAINER = "container"
        const val CURRENT_TAB = "currentTab"
        const val NAME = "fragmentName"
    }

}
