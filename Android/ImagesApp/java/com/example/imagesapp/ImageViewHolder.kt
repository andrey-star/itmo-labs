package com.example.imagesapp

import android.view.View
import androidx.recyclerview.widget.RecyclerView
import kotlinx.android.synthetic.main.list_item.view.*

class ImageViewHolder(val root: View) : RecyclerView.ViewHolder(root) {
    val imageDescription = root.image_description
    val imageUrl = root.image_url
}
