package ro.iugori.yadvs.model.rest;

import lombok.Getter;
import lombok.Setter;

@Getter

public class ErrorModel {

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
        code = String.format("%d (%s)", errCode.code, errCode.name());
    }

}
