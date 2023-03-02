using UnityEngine;

public class Stats : MonoBehaviour
{
    public static int money = 100;
    public int startMoney;
    public static int life = 10;
    public int startLife;
    void Start()
    {
        money = startMoney;
        life = startLife;
    }
    
}
