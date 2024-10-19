package ro.iugori.yadvs.model.error;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;

@NoArgsConstructor
@Getter
public class ErrorModel implements Serializable {

    private String code;
    @Setter
    private String message;
    @Setter
    private String moreInfo;
    private ErrorTarget target;

    public void setTarget(TargetType type, String name) {
        this.target = new ErrorTarget(type, name);
    }

    public void setCode(ErrorCode errCode) {
        code = String.format("%d: %s", errCode.code, errCode.name().toLowerCase());
    }

    public int getCodeAsInt() {
        return Integer.parseInt(getCode().split(":")[0]);
    }


    @Override
    public String toString() {
        return String.format("%6d: %s", getCodeAsInt(), message);
    }
}
