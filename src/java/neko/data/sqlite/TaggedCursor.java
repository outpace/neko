package neko.data.sqlite;

import android.database.Cursor;
import android.database.CursorWrapper;

public class TaggedCursor extends CursorWrapper {

    public final Object columns;

    public TaggedCursor(Cursor cursor, Object columns) {
        super(cursor);
        this.columns = columns;
    }

}
