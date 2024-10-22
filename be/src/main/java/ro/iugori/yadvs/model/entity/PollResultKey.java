package ro.iugori.yadvs.model.entity;

import lombok.Data;

import java.io.Serializable;

@Data
public class PollResultKey implements Serializable {

    private Long pollId;
    private Long optionId;

}
