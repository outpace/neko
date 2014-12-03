package neko.data.sqlite;

import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.content.Context;
import java.util.List;
import clojure.lang.IFn;

public class SQLiteHelper extends SQLiteOpenHelper {

    private final List<String> createQueriesList;
    private final List<String> dropTablesList;

    public SQLiteHelper(Context context, String name, int version,
                        List<String> createQueriesList,
                        List<String> dropTablesList) {
        super(context, name, null, version);
        this.createQueriesList = createQueriesList;
        this.dropTablesList = dropTablesList;
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        for (String q : createQueriesList) {
            db.execSQL(q);
        }
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        for (String q : dropTablesList) {
            db.execSQL(q);
        }
        this.onCreate(db);
    }
}
