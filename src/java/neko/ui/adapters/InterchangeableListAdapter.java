/*
* Copyright © 2012 Alexander Yakushev
* All rights reserved.
*
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License v1.0 which accompanies this
* distribution, and is available at
* <http://www.eclipse.org/legal/epl-v10.html>.
*
* By using this software in any fashion, you are agreeing to be bound by the
* terms of this license.  You must not remove this notice, or any other, from
* this software.
*/
package neko.ui.adapters;

import android.widget.BaseAdapter;
import android.view.View;
import android.view.ViewGroup;
import clojure.lang.IFn;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.ArrayList;

public class InterchangeableListAdapter extends BaseAdapter {

    private IFn createViewFn;
    private IFn updateViewFn;
    private List data;

    public InterchangeableListAdapter(IFn createViewFn, IFn updateViewFn,
                                      List initialData) {
        super();
        this.createViewFn = createViewFn;
        this.updateViewFn = updateViewFn;
        this.data = initialData;
    }

    public InterchangeableListAdapter(IFn createViewFn, IFn updateViewFn,
                                      Map initialData) {
        this(createViewFn, updateViewFn, new ArrayList(initialData.entrySet()));
    }

    public InterchangeableListAdapter(IFn createViewFn, IFn updateViewFn,
                                      Set initialData) {
        this(createViewFn, updateViewFn, new ArrayList(initialData));
    }

    @Override
    public int getCount() {
        return data.size();
    }
    @Override
    public Object getItem(int position) {
        return data.get(position);
    }
    @Override
    public long getItemId(int position) {
        return position;
    }
    @Override
    public boolean hasStableIds() {
        return false;
    }
    @Override
    public boolean isEmpty() {
        return data.isEmpty();
    }
    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        if (view == null) {
            view = (View)createViewFn.invoke(parent.getContext());
        }
        updateViewFn.invoke(position, view, parent, data.get(position));
        return view;
    }

    public void setData(List newData) {
        this.data = newData;
        notifyDataSetInvalidated();
    }

    public void setData(Set newData) {
      setData(new ArrayList(newData));
    }

    public void setData(Map newData) {
      setData(new ArrayList(newData.entrySet()));
    }
}
